package io.taig.crock

import cats.data.Validated
import cats.syntax.all.*
import io.taig.crock.schema.*
import io.taig.crock.validation.{Validation, Violation}
import io.circe.{Decoder as JsonDecoder, Json}

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

    def decode[A, B](
        fa: Primitive[A],
        validation: Validation[A, B],
        json: Json
    ): Validated[Violations, B] = decode(fa, json).andThen(validation(_).leftMap(Violations.root))

    def typeOf(json: Json): String =
      json.fold("null", _ => "boolean", _ => "number", _ => "string", _ => "array", _ => "object")

    override def decode[A](fa: Primitive[A], json: Json): Validated[Violations, A] = fa match
      case Primitive.Root(_, tpe) =>
        decode(tpe, json).fold(_ => Violations.rootNec(Violation.tpe(tpe.toString, typeOf(json))).invalid, _.valid)
      case Primitive.Validate(primitive, validation, _) => decode(primitive, validation, json)
