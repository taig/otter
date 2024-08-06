package io.taig.otter

import cats.parse.Parser
import cats.parse.strings.Json
import cats.data.NonEmptyList
import cats.parse.Parser0
import cats.parse.Numbers
import java.util.regex.Pattern

object ConstraintParser:
  def apply(value: String): Either[Parser.Error, Constraint.Any] = parser.parseAll(value)

  val parser: Parser[Constraint.Any] =
    val whitespace: Parser0[Unit] = Parser.charIn(" \t\r\n").void.rep0.void
    def nonEmptyList[A](parser: Parser[A]): Parser[NonEmptyList[A]] =
      parser.repSep(DataParser.listSeparator).surroundedBy(whitespace)
    val int: Parser[Int] = Numbers.signedIntString.mapFilter(_.toIntOption)

    val tpe: Parser[Constraint.Type] =
      Parser.string("type") *> whitespace *> Json.delimited.parser.map(Constraint.Type.apply)

    val oneOf: Parser[Constraint.OneOf] =
      Parser.string("oneOf") *>
        whitespace *>
        nonEmptyList(DataParser.primitive)
          .between(Parser.char('['), Parser.char(']'))
          .map(Constraint.OneOf.apply)

    val maxItems: Parser[Constraint.Collection.MaxItems] =
      Parser.string("maxItems") *> whitespace *> int.map(Constraint.Collection.MaxItems.apply)

    val minItems: Parser[Constraint.Collection.MinItems] =
      Parser.string("minItems") *> whitespace *> int.map(Constraint.Collection.MinItems.apply)

    val uniqueItems: Parser[Constraint.Collection.UniqueItems.type] =
      Parser.string("uniqueItems").as(Constraint.Collection.UniqueItems)

    val maxProperties: Parser[Constraint.Object.MaxProperties] =
      Parser.string("maxProperties") *> whitespace *> int.map(Constraint.Object.MaxProperties.apply)

    val minProperties: Parser[Constraint.Object.MinProperties] =
      Parser.string("minProperties") *> whitespace *> int.map(Constraint.Object.MinProperties.apply)

    val matches: Parser[Constraint.Primitive.Matches] =
      Parser.string("matches") *> whitespace *> Json.delimited.parser
        .map(Pattern.compile)
        .map(Constraint.Primitive.Matches.apply)

    val maximumInclusive: Parser[Constraint.Primitive.Maximum] =
      Parser.string("lteq") *> whitespace *> DataParser.number
        .map(Comparison(_, exclusive = false))
        .map(Constraint.Primitive.Maximum.apply)

    val maximumExclusive: Parser[Constraint.Primitive.Maximum] =
      Parser.string("lt") *> whitespace *> DataParser.number
        .map(Comparison(_, exclusive = true))
        .map(Constraint.Primitive.Maximum.apply)

    val minimumInclusive: Parser[Constraint.Primitive.Minimum] =
      Parser.string("gteq") *> whitespace *> DataParser.number
        .map(Comparison(_, exclusive = false))
        .map(Constraint.Primitive.Minimum.apply)

    val minimumExclusive: Parser[Constraint.Primitive.Minimum] =
      Parser.string("gt") *> whitespace *> DataParser.number
        .map(Comparison(_, exclusive = true))
        .map(Constraint.Primitive.Minimum.apply)

    val maxLength: Parser[Constraint.Primitive.MaxLength] =
      Parser.string("maxLength") *> whitespace *> int.map(Constraint.Primitive.MaxLength.apply)

    val minLength: Parser[Constraint.Primitive.MinLength] =
      Parser.string("minLength") *> whitespace *> int.map(Constraint.Primitive.MinLength.apply)

    val multiple: Parser[Constraint.Primitive.Multiple] =
      Parser.string("multiple") *> whitespace *> DataParser.number.map(Constraint.Primitive.Multiple.apply)

    Parser.oneOf(
      tpe ::
        oneOf ::
        maxItems ::
        minItems ::
        uniqueItems ::
        maxProperties ::
        minProperties ::
        matches ::
        maximumInclusive ::
        maximumExclusive ::
        minimumInclusive ::
        minimumExclusive ::
        maxLength ::
        minLength ::
        multiple ::
        Nil
    )
