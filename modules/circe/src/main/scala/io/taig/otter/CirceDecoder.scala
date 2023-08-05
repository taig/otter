package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.*
import io.taig.otter.validation.{Constraint, Violation}
import io.circe.{Decoder as JsonDecoder, Json}

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder:
    override def decode[A](schema: Schema[A], json: Json): Validated[Violations, A] = schema match
      case schema: Schema.Value[?] => value.decode(schema, json)
      case schema: Collection[?]   => collection.decode(schema, json)
      case schema: Record[?]       => ???
      case schema: Product[?]      => ???
      case schema: Coproduct[?]    => ???
      case schema: Dictionary[?]   => ???

  val value: Decoder[Schema.Value, Json] = new Decoder:
    override def decode[A](schema: Schema.Value[A], json: Json): Validated[Violations, A] = schema match
      case schema: Primitive[?]   => primitive.decode(schema, json)
      case schema: Enumeration[?] => enumeration.decode(schema, json)

  val primitive: Decoder[Primitive, Json] = new Decoder:
    def decode[A](json: Json): Type[A] => JsonDecoder.Result[A] =
      case Type.BigDecimal => json.as[BigDecimal]
      case Type.BigInt     => json.as[BigInt]
      case Type.Boolean    => json.as[Boolean]
      case Type.Double     => json.as[Double]
      case Type.Float      => json.as[Float]
      case Type.Int        => json.as[Int]
      case Type.Long       => json.as[Long]
      case Type.String     => json.as[String]

    override def decode[A](primitive: Primitive[A], json: Json): Validated[Violations, A] = primitive match
      case Schema.Primitive.Root(_, tpe) =>
        decode(json)(tpe).fold(_ => Violations.rootNec(Violation.tpe(tpe.toString, typeOf(json))).invalid, _.valid)
      case Schema.Primitive.Validate(self, validation, _) =>
        decode(self, json).andThen(validation(_).leftMap(Violations.root))
      case Schema.Primitive.Optional(self) =>
        if json.isNull then none.valid[Violations] else decode(self, json).map(_.some)

  val collection: Decoder[Collection.Of[Schema, *], Json] = new Decoder:
    override def decode[A](collection: Collection.Of[Schema, A], json: Json): Validated[Violations, A] =
      collection match
        case Schema.Collection.Root(of, _) =>
          json.asArray match
            case Some(json) =>
              Chain.fromSeq(json).zipWithIndex.traverse { case (json, index) =>
                CirceDecoder.schema.decode(of.value, json).leftMap(_.modifyHistory(index /: _))
              }
            case None => Violations.rootNec(Violation.tpe("array", typeOf(json))).invalid
        case Schema.Collection.Validate(self, validation, _) =>
          decode(self, json).andThen(validation(_).leftMap(Violations.root))
        case Schema.Collection.Optional(self) =>
          if json.isNull then none.valid[Violations] else decode(self, json).map(_.some)

  val enumeration: Decoder[Enumeration, Json] = new Decoder:
    override def decode[A](enumeration: Enumeration[A], json: Json): Validated[Violations, A] = enumeration match
      case Schema.Enumeration.Root(mapping, schema, _) =>
        CirceDecoder.value
          .decode(schema.value, json)
          .andThen: b =>
            mapping
              .prj(b)
              .toValid:
                val values = enumeration.values(StringEncoder.value).map(_.getOrElse("null"))
                val actual = json.fold("null", String.valueOf, _.toString, identity, _ => "array", _ => "object")
                Violations.rootNec(Violation(Constraint.OneOf(values), actual.some))
      case Schema.Enumeration.Validate(self, validation, _) =>
        decode(self, json).andThen(validation(_).leftMap(Violations.root))
      case Schema.Enumeration.Optional(self) =>
        if json.isNull then none.valid[Violations] else decode(self, json).map(_.some)

  def typeOf(json: Json): String =
    json.fold("null", _ => "boolean", _ => "number", _ => "string", _ => "array", _ => "object")
