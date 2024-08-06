package io.taig.otter

import cats.parse.strings.Json
import cats.parse.{Numbers, Parser, Parser0}
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object DataParser:
  def apply(value: String): Either[Parser.Error, Data] = parser.parseAll(value)

  val whitespace: Parser0[Unit] = Parser.charIn(" \t\r\n").void.rep0.void
  val nil = Parser.string("null").as(Data.Null)
  val boolean = Parser.string("true").as(Data.Boolean(true)).orElse(Parser.string("false").as(Data.Boolean(false)))
  val string = Json.delimited.parser.map(Data.String.apply)
  val number = Numbers.jsonNumber.map: value =>
    // TODO nasty
    if value.contains(".")
    then Data.Number(value.toFloatOption.orElse(value.toDoubleOption).getOrElse(JBigDecimal(value)))
    else Data.Number(value.toIntOption.orElse(value.toLongOption).getOrElse(JBigInteger(value)))
  val listSeparator: Parser[Unit] = Parser.char(',').soft.surroundedBy(whitespace).void
  def list[A](parser: Parser[A]): Parser0[List[A]] = parser.repSep0(listSeparator).surroundedBy(whitespace)
  val primitive: Parser[Data.Primitive] = Parser.oneOf(string :: number :: boolean :: Nil)

  val parser: Parser[Data] = Parser.recursive[Data]: recurse =>
    val array = list(recurse).with1
      .between(Parser.char('['), Parser.char(']'))
      .map(values => Data.Array(values.toVector))

    val keyValue: Parser[(String, Data)] =
      Json.delimited.parser ~ (Parser.char(':').surroundedBy(whitespace) *> recurse)

    val obj = list(keyValue).with1
      .between(Parser.char('{'), Parser.char('}'))
      .map(values => Data.Object(values.toVector))

    Parser.oneOf(primitive :: array :: obj :: nil :: Nil)
