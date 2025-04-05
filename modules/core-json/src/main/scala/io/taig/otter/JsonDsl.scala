package io.taig.otter

import cats.Order
import cats.kernel.Eq
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping

trait JsonDsl:
  val string: Json.Primitive.String[String] = Json.Primitive.String.invariant.string
  val int: Json.Primitive.Number[Int] = Json.Primitive.Number.invariant.int
  val long: Json.Primitive.Number[Long] = Json.Primitive.Number.invariant.long
  val float: Json.Primitive.Number[Float] = Json.Primitive.Number.invariant.float
  val double: Json.Primitive.Number[Double] = Json.Primitive.Number.invariant.double
  val boolean: Json.Primitive.Boolean[Boolean] = Json.Primitive.Boolean.invariant.boolean

  object key:
    val string: Json.Key[String] = Json.Key.Primitive.invariant.string

    object constant:
      export Json.Key.Constant.invariant.constant as apply
      def apply(value: String): Json.Key.Constant[String] = apply(string, value)

    export Json.Key.Union.invariant.branch

  export Json.Record.invariant.field

  def field[A](name: String, codec: => Json[A]): Json.Record[A] = field(name, key = key.string, value = codec)

  export Json.Union.invariant.branch

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
    export Json.Constant.invariant.constant as apply
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
  ): Json.Union[B] = ???
  // Json.Enumeration(Enumeration.Root(codec = Reference.later(codec), mapping, metadata = Metadata.Empty))

  def enumeration[A: Order, B](codec: => Json.Primitive[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Json.Union[B] = ??? // enumeration(codec)(using Mapping.enumeration(f))

object JsonDsl extends JsonDsl
