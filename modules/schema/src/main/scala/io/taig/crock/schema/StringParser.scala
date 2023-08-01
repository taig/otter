package io.taig.crock.schema

import cats.syntax.all.*
import cats.data.Validated
import io.taig.crock.validation.Violation

object StringParser extends Parser[Primitive, String]:
  def parse[B](value: String): Type[B] => Option[B] =
    case Type.String  => value.some
    case Type.Int     => value.toIntOption
    case Type.Long    => value.toLongOption
    case Type.Float   => value.toFloatOption
    case Type.Double  => value.toDoubleOption
    case Type.Boolean => value.toBooleanOption
    case Type.BigDecimal =>
      try BigDecimal(value).some
      catch case _: NumberFormatException => none
    case Type.BigInt =>
      try BigInt(value).some
      catch case _: NumberFormatException => none

  override def parse[B](schema: Primitive[B], value: String): Validated[Violations, B] = schema match
    case Primitive.Root(_, tpe) => parse(value)(tpe).toValid(Violations.rootNec(Violation.tpe(tpe.toString, value)))
    case Primitive.Validate(schema, validation, _) =>
      parse(schema, value).andThen(validation(_).leftMap(Violations.root))
