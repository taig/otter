package io.taig.crock

import cats.data.Validated
import cats.syntax.all.*
import io.taig.crock.schema.*
import io.taig.crock.validation.{Constraint, Violation}
import io.circe.{Decoder as JsonDecoder, Json}

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder[Schema, Json]:
    override def decode[B](schema: Schema[B], json: Json): Validated[Violations, B] = schema match
      case schema: Schema.Value[?] => value.decode(schema, json)
      case schema: Collection[?]   => collection.decode(schema, json)

  val value: Decoder[Schema.Value, Json] = new Decoder[Schema.Value, Json]:
    override def decode[B](schema: Schema.Value[B], json: Json): Validated[Violations, B] = schema match
      case schema: Primitive[?]   => primitive.decode(schema, json)
      case schema: Enumeration[?] => enumeration.decode(schema, json)

  val primitive: Decoder[Primitive, Json] = new Decoder[Primitive, Json]:
    def decode[B](json: Json): Type[B] => JsonDecoder.Result[B] =
      case Type.BigDecimal => json.as[BigDecimal]
      case Type.BigInt     => json.as[BigInt]
      case Type.Boolean    => json.as[Boolean]
      case Type.Double     => json.as[Double]
      case Type.Float      => json.as[Float]
      case Type.Int        => json.as[Int]
      case Type.Long       => json.as[Long]
      case Type.String     => json.as[String]

    override def decode[A](primitive: Primitive[A], json: Json): Validated[Violations, A] = primitive match
      case Primitive.Root(_, tpe) =>
        decode(json)(tpe).fold(_ => Violations.rootNec(Violation.tpe(tpe.toString, typeOf(json))).invalid, _.valid)
      case Primitive.Validate(self, validation, _) =>
        decode(self, json).andThen(validation(_).leftMap(Violations.root))
      case primitive: Primitive.Optional[?] => decode(primitive, json)

    def decode[A](primitive: Primitive.Optional[A], json: Json): Validated[Violations, Option[A]] =
      if json.isNull then none[A].valid else decode(primitive.self, json).map(_.some)

  val collection: Decoder[Collection, Json] = new Decoder[Collection, Json]:
    override def decode[B](collection: Collection[B], json: Json): Validated[Violations, B] = collection match
      case Collection.Root(of, _) =>
        json.asArray match
          case Some(json) =>
            json.zipWithIndex.traverse { case (json, index) =>
              CirceDecoder.schema.decode(of.value, json).leftMap(_.modifyHistory(index /: _))
            }
          case None => Violations.rootNec(Violation.tpe("array", typeOf(json))).invalid
      case collection: Collection.Validate[?, ?, ?] =>
        decode(collection.self, json).andThen(collection.validation(_).leftMap(Violations.root))
      case collection: Collection.Optional[?, ?] => decode(collection, json)

    def decode[B](collection: Collection.Optional[?, B], json: Json): Validated[Violations, Option[B]] =
      if json.isNull then none[B].valid else decode(collection.self, json).map(_.some)

  val enumeration: Decoder[Enumeration, Json] = new Decoder[Enumeration, Json]:
    override def decode[B](enumeration: Enumeration[B], json: Json): Validated[Violations, B] = enumeration match
      case Enumeration.Root(mapping, schema, _) =>
        CirceDecoder.value
          .decode(schema.value, json)
          .andThen: b =>
            mapping
              .prj(b)
              .toValid:
                val values = enumeration.values(StringEncoder.value).map(_.getOrElse("null"))
                val actual = json.fold("null", String.valueOf, _.toString, identity, _ => "array", _ => "object")
                Violations.rootNec(Violation(Constraint.OneOf(values), actual.some))
      case Enumeration.Validate(self, validation, _) =>
        decode(self, json).andThen(validation(_).leftMap(Violations.root))
      case enumeration: Enumeration.Optional[?] => decode(enumeration, json)

    def decode[B](enumeration: Enumeration.Optional[B], json: Json): Validated[Violations, Option[B]] =
      if json.isNull then none[B].valid else decode(enumeration.self, json).map(_.some)

  def typeOf(json: Json): String =
    json.fold("null", _ => "boolean", _ => "number", _ => "string", _ => "array", _ => "object")
