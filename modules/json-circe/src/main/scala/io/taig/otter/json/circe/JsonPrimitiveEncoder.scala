package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.taig.otter.Type
import io.circe.syntax.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonPrimitiveEncoder:
  def apply[A](schema: Primitive.Writer[A], a: A): Json = schema match
    case Base.Primitive.Optional(self)                  => a.map(apply(self, _)).getOrElse(Json.Null)
    case Base.Primitive.Required.Modify(self, _, f)     => apply(self, f(a))
    case Base.Primitive.Required.Writer.Modify(self, f) => apply(self, f(a))
    case Base.Primitive.Root(tpe)                       => apply(tpe, a)
    case Base.Primitive.Writer.Modify(self, f)          => apply(self, f(a))
    case Base.Primitive.Writer.Optional(self)           => a.map(apply(self, _)).getOrElse(Json.Null)

  def apply[A](tpe: Type[A], a: A): Json = tpe match
    case Type.BigDecimal => (a: JBigDecimal).asJson
    case Type.BigInteger => (a: JBigInteger).asJson
    case Type.Boolean    => (a: Boolean).asJson
    case Type.Double     => (a: Double).asJson
    case Type.Float      => (a: Float).asJson
    case Type.Int        => (a: Int).asJson
    case Type.Long       => (a: Long).asJson
    case Type.String     => (a: String).asJson
