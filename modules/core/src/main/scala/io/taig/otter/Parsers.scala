package io.taig.otter

import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.strings.Json

private[otter] object Parsers:
  val whitespace: Parser0[Unit] = Parser.charIn(" \t\r\n").void.rep0.void

  val colon: Parser[Unit] = Parser.char(':')

  val equals: Parser[Unit] = Parser.char('=')

  val int: Parser[Int] = Numbers.signedIntString.mapFilter(_.toIntOption)

  def brackets[A](parser: Parser.With1[A]) = parser.between(Parser.char('['), Parser.char(']'))

  val separator: Parser[Unit] = Parser.char(',').soft.surroundedBy(whitespace).void

  def list[A](parser: Parser[A]): Parser0[List[A]] = parser.repSep0(separator).surroundedBy(whitespace)

  val text: Parser[String] = Json.delimited.parser

  val token: Parser[String] = Parser.charsWhile: value =>
    (value >= 'a' && value <= 'z') ||
      (value >= 'A' && value <= 'Z') ||
      (value >= '0' && value <= '9')

  val step: Parser[Step] =
    val field: Parser[Step.Field] =
      Parser.char('.') *> token.map(Step.Field.apply)

    val index: Parser[Step.Index] = brackets(int.with1).map(Step.Index.apply)

    Parser.oneOf(field :: index :: Nil)

//   val xpath: Parser[XPath] = Parser.char('$') *> step.rep0.map(values => XPath(Chain.fromSeq(values)))

//   val constraint: Parser[Constraint] =
//     val tpe: Parser[Constraint.Type] =
//       Parser.string("type") *> whitespace *> Json.delimited.parser.map(Constraint.Type.apply)

//     val oneOf: Parser[Constraint.OneOf] =
//       Parser.string("oneOf") *>
//         whitespace *>
//         brackets(list(data.primitive).with1).map(Constraint.OneOf.apply)

//     val maxItems: Parser[Constraint.Collection.MaxItems] =
//       Parser.string("maxItems") *> whitespace *> int.map(Constraint.Collection.MaxItems.apply)

//     val minItems: Parser[Constraint.Collection.MinItems] =
//       Parser.string("minItems") *> whitespace *> int.map(Constraint.Collection.MinItems.apply)

//     val uniqueItems: Parser[Constraint.Collection.UniqueItems.type] =
//       Parser.string("uniqueItems").as(Constraint.Collection.UniqueItems)

//     val maxProperties: Parser[Constraint.Object.MaxProperties] =
//       Parser.string("maxProperties") *> whitespace *> int.map(Constraint.Object.MaxProperties.apply)

//     val minProperties: Parser[Constraint.Object.MinProperties] =
//       Parser.string("minProperties") *> whitespace *> int.map(Constraint.Object.MinProperties.apply)

//     val matches: Parser[Constraint.Primitive.Matches] =
//       Parser.string("matches") *> whitespace *> Json.delimited.parser
//         .map(Pattern.compile)
//         .map(Constraint.Primitive.Matches.apply)

//     val maximumInclusive: Parser[Constraint.Primitive.Maximum] =
//       Parser.string("lteq") *> whitespace *> data.number
//         .map(Comparison(_, exclusive = false))
//         .map(Constraint.Primitive.Maximum.apply)

//     val maximumExclusive: Parser[Constraint.Primitive.Maximum] =
//       Parser.string("lt") *> whitespace *> data.number
//         .map(Comparison(_, exclusive = true))
//         .map(Constraint.Primitive.Maximum.apply)

//     val minimumInclusive: Parser[Constraint.Primitive.Minimum] =
//       Parser.string("gteq") *> whitespace *> data.number
//         .map(Comparison(_, exclusive = false))
//         .map(Constraint.Primitive.Minimum.apply)

//     val minimumExclusive: Parser[Constraint.Primitive.Minimum] =
//       Parser.string("gt") *> whitespace *> data.number
//         .map(Comparison(_, exclusive = true))
//         .map(Constraint.Primitive.Minimum.apply)

//     val maxLength: Parser[Constraint.Primitive.MaxLength] =
//       Parser.string("maxLength") *> whitespace *> int.map(Constraint.Primitive.MaxLength.apply)

//     val minLength: Parser[Constraint.Primitive.MinLength] =
//       Parser.string("minLength") *> whitespace *> int.map(Constraint.Primitive.MinLength.apply)

//     val multiple: Parser[Constraint.Primitive.Multiple] =
//       Parser.string("multiple") *> whitespace *> data.number.map(Constraint.Primitive.Multiple.apply)

//     Parser.oneOf(
//       tpe ::
//         oneOf ::
//         maxItems ::
//         minItems ::
//         uniqueItems ::
//         maxProperties ::
//         minProperties ::
//         matches ::
//         maximumInclusive ::
//         maximumExclusive ::
//         minimumInclusive ::
//         minimumExclusive ::
//         maxLength ::
//         minLength ::
//         multiple ::
//         Nil
//     )

//   val violation: Parser[Violation] =
//     ((Parsers.constraint.with1 <* Parser.string(" ! ")) ~ data.root).map(Violation.apply)

//   def indexed[A](self: Parser[A]): Parser[Indexed[A]] = ((xpath <* colon <* whitespace) ~ self).map(Indexed.apply)
