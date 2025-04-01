package io.taig.otter

import cats.implicits.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

trait JsonCodecs extends Primitives.Default[Json]:
  override protected def lift[A](value: Primitive[A]): Json[A] = Json(value)

// object collection extends Collections[Json]:
//   override protected def lift[A](codec: Collection[Json, A]): Json[A] = Json(codec)

object JsonCodecs extends JsonCodecs
