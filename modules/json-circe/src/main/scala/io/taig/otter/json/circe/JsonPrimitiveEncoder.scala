package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Primitive
import io.taig.otter.Type
import io.circe.syntax.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Plain.*

object JsonPrimitiveEncoder:
  def apply[A](data: Base.Primitive[A], a: A): Json = data match
    case Base.Primitive.Root(tpe) => apply(tpe, a)

  def apply[A](tpe: Type[A], a: A): Json = tpe match
    case Type.BigDecimal => (a: JBigDecimal).asJson
    case Type.BigInteger => (a: JBigInteger).asJson
    case Type.Boolean    => (a: Boolean).asJson
    case Type.Double     => (a: Double).asJson
    case Type.Float      => (a: Float).asJson
    case Type.Int        => (a: Int).asJson
    case Type.Long       => (a: Long).asJson
    case Type.String     => (a: String).asJson
