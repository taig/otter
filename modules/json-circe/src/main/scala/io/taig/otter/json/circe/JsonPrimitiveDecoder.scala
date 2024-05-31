package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter as Base
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonPrimitiveDecoder:
  def apply[A](schema: Base.Primitive[A], json: Json): Validated[Violations[Json, Json], A] =
    apply(schema.tpe, json).toValidated.leftMap: _ =>
      Violations.rootNec(Violation.tpe(typeOf(schema.tpe), typeOf(json)).map(_.asJson))

  def apply[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[JBigDecimal]
    case Type.BigInteger => json.as[JBigInteger]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]
