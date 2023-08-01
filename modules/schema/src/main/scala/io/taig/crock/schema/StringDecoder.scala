package io.taig.crock.schema

import cats.syntax.all.*
import cats.data.Validated
import io.taig.crock.validation.Violation

object StringDecoder extends Decoder[Primitive, Option[String]]:
  override def decode[B](schema: Primitive[B], value: Option[String]): Validated[Violations, B] = schema match
    case Primitive.Root(_, tpe) =>
      value match
        case Some(value) => decode(value)(tpe).toValid(Violations.rootNec(Violation.tpe(tpe.toString, value)))
        case None        => Violations.rootNec(Violation.required).invalid
    case Primitive.Validate(self, validation, _) =>
      decode(self, value).andThen(validation(_).leftMap(Violations.root))
    case schema: Primitive.Optional[?] => decode(schema, value)

  def decode[B](schema: Primitive.Optional[B], value: Option[String]): Validated[Violations, Option[B]] =
    value.fold(none[B].valid)(_ => decode(schema.self, value).map(_.some))

  def decode[B](value: String): Type[B] => Option[B] =
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
