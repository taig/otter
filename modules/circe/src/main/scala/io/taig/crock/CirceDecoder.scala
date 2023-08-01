package io.taig.crock

import cats.data.Validated
import cats.syntax.all.*
import io.taig.crock.schema.*
import io.taig.crock.validation.Violation
import io.circe.{Decoder as JsonDecoder, Json}

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder[Schema, Json]:
    override def decode[B](schema: Schema[B], json: Json): Validated[Violations, B] = schema match
      case schema: Primitive[?]  => primitive.decode(schema, json)
      case schema: Collection[?] => collection.decode(schema, json)
      case _                     => ???

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

    override def decode[A](schema: Primitive[A], json: Json): Validated[Violations, A] = schema match
      case Primitive.Root(_, tpe) =>
        decode(json)(tpe).fold(_ => Violations.rootNec(Violation.tpe(tpe.toString, typeOf(json))).invalid, _.valid)
      case Primitive.Validate(schema, validation, _) =>
        decode(schema, json).andThen(validation(_).leftMap(Violations.root))

  val collection: Decoder[Collection, Json] = new Decoder[Collection, Json]:
    override def decode[B](schema: Collection[B], json: Json): Validated[Violations, B] = schema match
      case Collection.Root(of, _) =>
        json.asArray match
          case Some(json) =>
            json.zipWithIndex.traverse { case (json, index) =>
              CirceDecoder.schema.decode(of.value, json).leftMap(_.modifyHistory(index /: _))
            }
          case None => Violations.rootNec(Violation.tpe("array", typeOf(json))).invalid
      case Collection.Validate(schema, validation, _) =>
        decode(schema, json).andThen(validation(_).leftMap(Violations.root))

  def typeOf(json: Json): String =
    json.fold("null", _ => "boolean", _ => "number", _ => "string", _ => "array", _ => "object")
