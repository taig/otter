package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Primitive
import io.taig.otter.Type
import io.circe.syntax.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonPrimitiveEncoder:
  def apply[A](schema: Primitive.Writer[A], a: A): Json = schema match
    case Primitive.Modify(self, _, f)              => modify(self, f, a)
    case Primitive.Optional(self)                  => optional(self, a)
    case Primitive.Required.Modify(self, _, f)     => modify(self, f, a)
    case Primitive.Required.Writer.Modify(self, f) => modify(self, f, a)
    case Primitive.Required.Root(tpe)              => apply(tpe, a)
    case Primitive.Writer.Modify(self, f)          => modify(self, f, a)
    case Primitive.Writer.Optional(self)           => optional(self, a)

  def modify[A, B](schema: Primitive.Writer[A], f: B => A, b: B): Json = apply(schema, f(b))

  def optional[A](schema: Primitive.Writer[A], a: Option[A]): Json = a.map(apply(schema, _)).getOrElse(Json.Null)

  def apply[A](tpe: Type[A], a: A): Json = tpe match
    case Type.BigDecimal => (a: JBigDecimal).asJson
    case Type.BigInteger => (a: JBigInteger).asJson
    case Type.Boolean    => (a: Boolean).asJson
    case Type.Double     => (a: Double).asJson
    case Type.Float      => (a: Float).asJson
    case Type.Int        => (a: Int).asJson
    case Type.Long       => (a: Long).asJson
    case Type.String     => (a: String).asJson
