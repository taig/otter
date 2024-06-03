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
    case Base.Primitive.Optional(self)                  => optional(self, a)
    case Base.Primitive.Modify(self, _, f)              => modify(self, f, a)
    case Base.Primitive.Required.Modify(self, _, f)     => modify(self, f, a)
    case Base.Primitive.Required.Writer.Modify(self, f) => modify(self, f, a)
    case Base.Primitive.Required.Root(tpe)              => root(tpe, a)
    case Base.Primitive.Writer.Modify(self, f)          => modify(self, f, a)
    case Base.Primitive.Writer.Optional(self)           => optional(self, a)

  def modify[A, B](self: Primitive.Writer[A], f: B => A, b: B): Json = apply(self, f(b))

  def optional[A](self: Primitive.Writer[A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)

  def root[A](tpe: Type[A], a: A): Json = tpe match
    case Type.BigDecimal => (a: JBigDecimal).asJson
    case Type.BigInteger => (a: JBigInteger).asJson
    case Type.Boolean    => (a: Boolean).asJson
    case Type.Double     => (a: Double).asJson
    case Type.Float      => (a: Float).asJson
    case Type.Int        => (a: Int).asJson
    case Type.Long       => (a: Long).asJson
    case Type.String     => (a: String).asJson
