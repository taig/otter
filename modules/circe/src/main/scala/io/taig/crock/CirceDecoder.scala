package io.taig.crock

import cats.data.Validated
import cats.syntax.all.*
import io.taig.crock.schema.*
import io.taig.crock.validation.Violation
import io.circe.{Decoder as JsonDecoder, Json}

object CirceDecoder:
  val primitive: Decoder[Primitive, Json] = new Decoder[Primitive, Json]:
    def typeOf(json: Json): String =
      json.fold("null", _ => "boolean", _ => "number", _ => "string", _ => "array", _ => "object")

    def decode[B](json: Json): Type[B] => JsonDecoder.Result[B] =
      case Type.BigDecimal => json.as[BigDecimal]
      case Type.BigInt     => json.as[BigInt]
      case Type.Boolean    => json.as[Boolean]
      case Type.Double     => json.as[Double]
      case Type.Float      => json.as[Float]
      case Type.Int        => json.as[Int]
      case Type.Long       => json.as[Long]
      case Type.String     => json.as[String]

    override def decode[A](fa: Primitive[A], json: Json): Validated[Violations, A] = fa match
      case Primitive.Root(_, tpe) =>
        decode(json)(tpe).fold(_ => Violations.rootNec(Violation.tpe(tpe.toString, typeOf(json))).invalid, _.valid)
      case Primitive.Validate(schema, validation, _) =>
        decode(schema, json).andThen(validation(_).leftMap(Violations.root))
