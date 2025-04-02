package io.taig.otter

import cats.implicits.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import io.taig.otter.Json.Key

trait JsonCodecs extends Fields[Json.Key, Json], Primitives.Defaults[Json.Primitive]:
  override protected def lift[A](codec: Primitive[A]): Json.Primitive[A] = Json.Primitive(self = codec)

  override def field[A, B](
      name: A,
      key: => Primitive.String[A],
      value: => Json[B]
  ): Field.Required[Json.Key, Json, B] = Field.Required.Root(
    key = Reference.Constant(self = Reference.later(key), value = name),
    value = Reference.later(value),
    metadata = Metadata.Empty
  )

  // object collection extends Collections.Defaults[Json.Collection]:
  //   override protected def lift[A](codec: Collection[Json, A]): Json.Collection[A] = Json.Collection(self = codec)

// object JsonCodecs extends JsonCodecs
