package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Type
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import io.taig.otter.validation.Violations
import io.taig.otter as Base
import io.taig.otter.Plain.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Decoder
import io.taig.otter.validation.Validation

object JsonTypeDecoder:
  def apply[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[JBigDecimal]
    case Type.BigInteger => json.as[JBigInteger]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]
