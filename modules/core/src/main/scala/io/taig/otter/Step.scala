package io.taig.otter

import cats.Order
import cats.Show
import cats.parse.Parser
import cats.syntax.all.*
import cats.parse.Numbers

enum Step:
  case Field(name: String)
  case Index(value: Int)

  final override def toString: String = this match
    case Step.Field(field) => s".$field"
    case Step.Index(index) => s"[$index]"

object Step:
  private val parser: Parser[Step] =
    val token: Parser[String] = Parser.charsWhile: value =>
      (value >= 'a' && value <= 'z') ||
        (value >= 'A' && value <= 'Z') ||
        (value >= '0' && value <= '9')

    val field: Parser[Step.Field] =
      Parser.char('.') *> token.map(Step.Field.apply)

    val int: Parser[Int] = Numbers.signedIntString.mapFilter(_.toIntOption)

    val index: Parser[Step.Index] = int.with1.between(Parser.char('['), Parser.char(']')).map(Step.Index.apply)

    Parser.oneOf(field :: index :: Nil)

  def parse(value: String): Either[Parser.Error, Step] = parser.parseAll(value)

  given Order[Step] =
    case (Field(x), Field(y)) => x.compare(y)
    case (Index(x), Index(y)) => x.compare(y)
    case (Field(_), Index(_)) => 1
    case (Index(_), Field(_)) => -1

  given Show[Step] = Show.fromToString
