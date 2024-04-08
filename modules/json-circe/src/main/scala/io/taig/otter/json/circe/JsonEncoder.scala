package io.taig.otter.json.circe

import io.taig.otter.Encoder
import io.circe.Json
import io.taig.otter.Primitive
import io.taig.otter.Tuple
import io.taig.otter.Type
import io.circe.syntax.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonEncoder extends Encoder[Json]:
  override def apply[A](schema: Primitive[?, A], value: A): Json = schema match
    case Primitive.Required.Root(_, tpe)            => apply(tpe, value)
    case Primitive.Required.Modify(primitive, _, g) => apply(primitive, g(value))
    case Primitive.Optional.Root(primitive)         => value.map(apply(primitive, _)).getOrElse(Json.Null)
    case Primitive.Optional.Modify(primitive, _, g) => apply(primitive, g(value))

  def apply[A](tpe: Type[A], value: A): Json = tpe match
    case Type.BigDecimal => (value: JBigDecimal).asJson
    case Type.BigInteger => (value: JBigInteger).asJson
    case Type.Boolean    => (value: Boolean).asJson
    case Type.Double     => (value: Double).asJson
    case Type.Float      => (value: Float).asJson
    case Type.Int        => (value: Int).asJson
    case Type.Long       => (value: Long).asJson
    case Type.String     => (value: String).asJson

  override def apply[A](schema: Tuple[?, A], value: A): Json =
    JsonTupleEncoder(schema, value).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
