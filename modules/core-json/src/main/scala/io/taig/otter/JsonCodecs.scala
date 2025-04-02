package io.taig.otter

import cats.implicits.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import io.taig.otter.Json.Key
import io.taig.otter.Field.Required

trait JsonCodecs
    extends Fields[Json.Key, Json],
      Primitives.Numbers.Defaults[Json.Primitive.Number],
      Primitives.Booleans.Defaults[Json.Primitive.Boolean],
      Primitives.Strings.Defaults[Json.Primitive.String]:
  override protected def lift[A](codec: Primitive.Boolean[A]): Json.Primitive.Boolean[A] = Json.Primitive.Boolean(codec)
  override protected def lift[A](codec: Primitive.Number[A]): Json.Primitive.Number[A] = Json.Primitive.Number(codec)
  override protected def lift[A](codec: Primitive.String[A]): Json.Primitive.String[A] = Json.Primitive.String(codec)

  override def field[A, B](name: A, key: => Json.Primitive.String[A], value: => Json[B]): Required[Key, Json, B] =
    Field.Required.Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      metadata = Metadata.Empty
    )

  // object collection extends Collections.Defaults[Json.Collection]:
  //   override protected def lift[A](codec: Collection[Json, A]): Json.Collection[A] = Json.Collection(self = codec)

object JsonCodecs extends JsonCodecs
