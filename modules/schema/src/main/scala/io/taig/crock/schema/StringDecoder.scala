package io.taig.crock.schema

import cats.syntax.all.*
import cats.data.Validated
import io.taig.crock.validation.{Constraint, Violation}

object StringDecoder:
  val value: Decoder[Schema.Value, Option[String]] = new Decoder:
    override def decode[B](schema: Schema.Value[B], value: Option[String]): Validated[Violations, B] = schema match
      case schema: Primitive[?]   => primitive.decode(schema, value)
      case schema: Enumeration[?] => enumeration.decode(schema, value)

  val primitive: Decoder[Primitive, Option[String]] = new Decoder:
    override def decode[B](primitive: Primitive[B], value: Option[String]): Validated[Violations, B] = primitive match
      case Primitive.Root(_, tpe) =>
        value match
          case Some(value) => decode(value)(tpe).toValid(Violations.rootNec(Violation.tpe(tpe.toString, value)))
          case None        => Violations.rootNec(Violation.required).invalid
      case Primitive.Validate(self, validation, _) =>
        decode(self, value).andThen(validation(_).leftMap(Violations.root))
      case primitive: Primitive.Optional[?] => decode(primitive, value)

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

  val enumeration: Decoder[Enumeration, Option[String]] = new Decoder:
    override def decode[A](enumeration: Enumeration[A], value: Option[String]): Validated[Violations, A] =
      enumeration match
        case Enumeration.Root(mapping, schema, _) =>
          StringDecoder.value
            .decode(schema.value, value)
            .andThen: b =>
              mapping
                .prj(b)
                .toValid:
                  val values = enumeration.values(StringEncoder.value).map(_.getOrElse("null"))
                  val actual = value.getOrElse("null")
                  Violations.rootNec(Violation(Constraint.OneOf(values), actual.some))
        case Enumeration.Validate(self, validation, _) =>
          decode(self, value).andThen(validation(_).leftMap(Violations.root))
        case enumeration: Enumeration.Optional[?] => decode(enumeration, value)

    def decode[B](schema: Enumeration.Optional[B], value: Option[String]): Validated[Violations, Option[B]] =
      value.fold(none[B].valid)(_ => decode(schema.self, value).map(_.some))
