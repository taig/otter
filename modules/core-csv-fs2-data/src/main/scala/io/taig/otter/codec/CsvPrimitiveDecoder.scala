package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Csv
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.validation.Validation
import io.taig.validation.Violation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object CsvPrimitiveDecoder extends Decoder[Csv.Primitive.Node, String]:
  override def decode[R](csv: Csv.Primitive.Node[Nothing, R], value: String): Validated[Violations, R] = csv match
    case Csv.Primitive.Boolean.Schema(annotation) => decode(annotation.self, value)
    case Csv.Primitive.Number.Schema(annotation)  => decode(annotation.self, value)
    case Csv.Primitive.Text.Schema(annotation)    => decode(annotation.self, value)

  def decode[R](schema: Primitive[Nothing, R], value: String): Validated[Violations, R] = schema match
    case Primitive.Modify(self, f, _)         => decode(self, value).map(f)
    case Primitive.Boolean.Modify(self, f, _) => decode(self, value).map(f)
    case Primitive.Boolean.Root               =>
      value.toBooleanOption.toValid(mismatch("boolean", value)).leftMap(Violations.apply)
    case Primitive.Number.BigDecimal(validation) => number("bigDecimal", value, bigDecimal, validation)
    case Primitive.Number.BigInteger(validation) => number("bigInteger", value, bigInteger, validation)
    case Primitive.Number.Double(validation)     => number("double", value, _.toDoubleOption, validation)
    case Primitive.Number.Float(validation)      => number("float", value, _.toFloatOption, validation)
    case Primitive.Number.Int(validation)        => number("int", value, _.toIntOption, validation)
    case Primitive.Number.Long(validation)       => number("long", value, _.toLongOption, validation)
    case Primitive.Number.Modify(self, f, _)     => decode(self, value).map(f)
    case Primitive.Text.Format(name, parse, _)   =>
      parse(value).toValidated
        .leftMap(error => Violation(constraint = Constraint.Generic.Type(name), actual = value, hint = error.some))
        .leftMap(Violations.apply)
    case Primitive.Text.Modify(self, f, _) => decode(self, value).map(f)

    /** A cell is already text, so there is nothing to extract and no type for it to be the wrong one of. */
    case Primitive.Text.Root(validation) => validation.validate(value).toInvalid(value).leftMap(Violations.apply)

  /** Reports the cell that failed rather than the name of its type. Every CSV cell is text, so saying so carries no
    * information; the text itself is the only thing that tells the reader what went wrong.
    */
  private def mismatch(name: String, value: String): Violation[Constraint] =
    Violation(constraint = Constraint.Generic.Type(name), actual = value, hint = none)

  private def bigDecimal(value: String): Option[JBigDecimal] =
    Either.catchOnly[NumberFormatException](new JBigDecimal(value)).toOption

  private def bigInteger(value: String): Option[JBigInteger] =
    Either.catchOnly[NumberFormatException](new JBigInteger(value)).toOption

  private def number[A](
      name: String,
      value: String,
      parse: String => Option[A],
      validation: Validation[Constraint.Primitive.Number, A]
  ): Validated[Violations, A] =
    parse(value)
      .toValid(mismatch(name, value))
      .leftMap(Violations.apply)
      .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
