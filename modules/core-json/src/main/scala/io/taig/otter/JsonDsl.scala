package io.taig.otter

import io.taig.enumeration.ext.Mapping
import cats.Order
import io.taig.enumeration.ext.EnumerationValues
import cats.kernel.Eq

trait JsonDsl:
  val string: Json.Primitive.String[String] = Json.Primitive.String.invariant.string
  val int: Json.Primitive.Number[Int] = Json.Primitive.Number.invariant.int
  val long: Json.Primitive.Number[Long] = Json.Primitive.Number.invariant.long
  val float: Json.Primitive.Number[Float] = Json.Primitive.Number.invariant.float
  val double: Json.Primitive.Number[Double] = Json.Primitive.Number.invariant.double
  val boolean: Json.Primitive.Boolean[Boolean] = Json.Primitive.Boolean.invariant.boolean

  given fieldInvariant[Key[_], Value[_], Record[_]](using
      RecordInvariant[Record, Key, Value]
  ): FieldInvariant[Key, Value, Record] = new FieldInvariant[Key, Value, Record] {}

  object field extends FieldDsl[Json.Key, Json, Json.Record](key = string)

  object collection:
    private val invariant = Json.Collection.invariant
    export invariant.{
      chain,
      list,
      nonEmptyChain,
      nonEmptyList,
      nonEmptySeq,
      nonEmptySet,
      nonEmptyVector,
      seq,
      set,
      sortedSet,
      vector
    }

  object constant:
    def apply[A: Eq](codec: => Json.Primitive[A], a: A): Json.Constant[A] =
      Json.Constant(Constant.Root(codec = Reference.later(codec), reference = a, metadata = Metadata.Empty))
    def apply(value: String): Json.Constant[String] = apply(string, value)
    def apply(value: Int): Json.Constant[Int] = apply(int, value)
    def apply(value: Long): Json.Constant[Long] = apply(long, value)
    def apply(value: Float): Json.Constant[Float] = apply(float, value)
    def apply(value: Double): Json.Constant[Double] = apply(double, value)
    def apply(value: Boolean): Json.Constant[Boolean] = apply(boolean, value)

  object dictionary:
    private val invariant = Json.Dictionary.invariant
    export invariant.{
      chain,
      list,
      nonEmptyChain,
      nonEmptyList,
      nonEmptyMap,
      nonEmptySeq,
      nonEmptyVector,
      seq,
      sortedMap,
      vector
    }

  def enumeration[A, B](codec: => Json.Primitive[A])(using
      mapping: Mapping[B, A]
  ): Json.Enumeration[B] =
    Json.Enumeration(Enumeration.Root(codec = Reference.later(codec), mapping, metadata = Metadata.Empty))

  def enumeration[A: Order, B](codec: => Json.Primitive[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Json.Enumeration[B] = enumeration(codec)(using Mapping.enumeration(f))

object JsonDsl extends JsonDsl
