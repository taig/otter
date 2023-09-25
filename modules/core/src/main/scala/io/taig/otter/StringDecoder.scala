package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.Schema.{Collection, Primitive}
import io.taig.otter.validation.{Violation, Violations}

object StringDecoder:
  val value: Decoder[Schema.Value, Option[String]] = new Decoder:
    override def decode[B](schema: Schema.Value[B], value: Option[String]): Validated[Violations, B] = schema match
      case schema: Schema.Enumeration[?] => ???
      case schema: Schema.Primitive[?]   => primitive.decode(schema, value)

  val collection: Decoder[Schema.Collection[Schema.Value, *], Option[Chain[String]]] = new Decoder:
    override def decode[A](
        schema: Schema.Collection[Schema.Value, A],
        value: Option[Chain[String]]
    ): Validated[Violations, A] = schema match
      case Collection.Root(schema, _, _) =>
        Validated
          .fromOption(value, Violations.rootNec(Violation.required))
          .andThen(_.zipWithIndex.traverse { case (value, index) =>
            StringDecoder.value.decode(schema, value.some).leftMap(_.modifyHistory(index /: _))
          })
      case Collection.Optional(self) => value.traverse(values => decode(self, values.some))
      case Collection.Validate(self, validation, _) =>
        decode(self, value).andThen(validation(_).leftMap(Violations.root))

  val primitive: Decoder[Schema.Primitive, Option[String]] = new Decoder[Schema.Primitive, Option[String]]:
    override def decode[A](schema: Schema.Primitive[A], value: Option[String]): Validated[Violations, A] =
      (schema, value) match
        case (Primitive.Optional(_), None)           => none.valid[Violations]
        case (_, None)                               => Violations.rootNec(Violation.required).invalid
        case (Primitive.Optional(self), Some(value)) => decode(self, value).map(_.some)
        case (schema, Some(value))                   => decode(schema, value)

    def decode[A](schema: Schema.Primitive[A], value: String): Validated[Violations, A] = schema match
      case Primitive.Root(tpe, _, _, _) => ???
      case Primitive.Optional(self)     => decode(self, value).map(_.some)
      case Primitive.Validate(self, validation, _) =>
        decode(self, value).andThen(validation(_).leftMap(Violations.root))

    def decode[A](tpe: Type[A], value: String): Option[A] = tpe match
      case Type.BigDecimal =>
        try Some(BigDecimal(value))
        catch { case _: NumberFormatException => None }
      case Type.BigInt =>
        try Some(BigInt(value))
        catch { case _: NumberFormatException => None }
      case Type.Boolean => value.toBooleanOption
      case Type.Double  => value.toDoubleOption
      case Type.Float   => value.toFloatOption
      case Type.Int     => value.toIntOption
      case Type.Long    => value.toLongOption
      case Type.String  => value.some
