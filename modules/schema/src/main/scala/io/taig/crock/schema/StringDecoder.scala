package io.taig.crock.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.crock.validation.{Constraint, Violation}
import io.taig.crock.schema.schemas.*

object StringDecoder:
  val value: Decoder[Schema.Value, Option[String]] = new Decoder:
    override def decode[A](schema: Schema.Value[A], value: Option[String]): Validated[Violations, A] = schema match
      case schema: Primitive[?]   => primitive.decode(schema, value)
      case schema: Enumeration[?] => enumeration.decode(schema, value)

  val primitive: Decoder[Primitive, Option[String]] = new Decoder:
    override def decode[A](primitive: Primitive[A], value: Option[String]): Validated[Violations, A] = primitive match
      case Schema.Primitive.Root(_, tpe) =>
        value match
          case Some(value) => decode(value)(tpe).toValid(Violations.rootNec(Violation.tpe(tpe.toString, value)))
          case None        => Violations.rootNec(Violation.required).invalid
      case Schema.Primitive.Validate(self, validation, _) =>
        decode(self, value).andThen(validation(_).leftMap(Violations.root))
      case Schema.Primitive.Optional(self) => value.fold(none.valid[Violations])(_ => decode(self, value).map(_.some))

    def decode[A](value: String): Type[A] => Option[A] =
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
        case Schema.Enumeration.Root(mapping, schema, _) =>
          StringDecoder.value
            .decode(schema.value, value)
            .andThen: b =>
              mapping
                .prj(b)
                .toValid:
                  val values = enumeration.values(StringEncoder.value).map(_.getOrElse("null"))
                  val actual = value.getOrElse("null")
                  Violations.rootNec(Violation(Constraint.OneOf(values), actual.some))
        case Schema.Enumeration.Validate(self, validation, _) =>
          decode(self, value).andThen(validation(_).leftMap(Violations.root))
        case Schema.Enumeration.Optional(self) =>
          value.fold(none.valid[Violations])(_ => decode(self, value).map(_.some))

  val collection: Decoder[Collection.Of[Value, *], Chain[String]] = new Decoder:
    override def decode[A](
        collection: Collection.Of[Value, A],
        values: Chain[String]
    ): Validated[Violations, A] = collection match
      case Schema.Collection.Root(schema, _) =>
        values.zipWithIndex.traverse { case (value, index) =>
          StringDecoder.value.decode(schema.value, value.some).leftMap(_.modifyHistory(index /: _))
        }
      case Schema.Collection.Validate(self, validation, _) =>
        decode(self, values).andThen(validation(_).leftMap(Violations.root))
      case Schema.Collection.Optional(self) =>
        if values.isEmpty then none.valid[Violations] else decode(self, values).map(_.some)
