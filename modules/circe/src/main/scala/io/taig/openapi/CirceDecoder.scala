package io.taig.openapi

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.circe.Json
import io.taig.openapi.schema.{Decoder, Primitive, Type}

object CirceDecoder:
  val primitive: Decoder[Primitive, Json] = new Decoder[Primitive, Json]:
    def decode[B](tpe: Type[B], json: Json): ValidatedNec[String, B] = tpe match
      case Type.BigDecimal => json.as[BigDecimal].toValidatedNec.leftMap(_.map(_.toString))
      case Type.BigInt => ???
      case Type.Boolean => ???
      case Type.Double => ???
      case Type.Float => ???
      case Type.Int => ???
      case Type.Long => ???
      case Type.String => ???

    override def decode[B](fa: Primitive[B], json: Json): ValidatedNec[String, B] = fa match
      case Primitive.Root(_, _, _, tpe) => decode(tpe, json)
      case Primitive.Validate(primitive, validation, _) =>
        decode(primitive, json).andThen(_ => ???)
