package io.taig.openapi

import cats.data.Validated
import cats.syntax.all.*
import io.circe.{Decoder as JsonDecoder, Json}
import io.taig.openapi.schema.{Decoder, Primitive, Type, Violations}
import io.taig.openapi.validation.Violation

object CirceDecoder:
  val primitive: Decoder[Primitive, Json] = new Decoder[Primitive, Json]:
    def decode[B](tpe: Type[B], json: Json): JsonDecoder.Result[B] = tpe match
      case Type.BigDecimal => json.as[BigDecimal]
      case Type.BigInt     => json.as[BigInt]
      case Type.Boolean    => json.as[Boolean]
      case Type.Double     => json.as[Double]
      case Type.Float      => json.as[Float]
      case Type.Int        => json.as[Int]
      case Type.Long       => json.as[Long]
      case Type.String     => json.as[String]

    override def decode[B](fa: Primitive[B], json: Json): Validated[Violations[Json], B] = fa match
      case Primitive.Root(_, _, _, tpe) =>
        decode(tpe, json).fold(_ => Violations.rootNec(Violation.tpe(tpe.toString, json)).invalid, _.valid)
      case Primitive.Validate(primitive, validation, _) =>
        decode(primitive, json).andThen(_ => ???)
