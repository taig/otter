package io.taig.otter

import cats.Order
import cats.kernel.Eq
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping

trait JsonDsl:
  val string: Json.Primitive[String] = Json.Primitive.syntax.string
  val int: Json.Primitive[Int] = Json.Primitive.syntax.int
  val long: Json.Primitive[Long] = Json.Primitive.syntax.long
  val float: Json.Primitive[Float] = Json.Primitive.syntax.float
  val double: Json.Primitive[Double] = Json.Primitive.syntax.double
  val boolean: Json.Primitive[Boolean] = Json.Primitive.syntax.boolean

  object key:
    val string: Json.Key.Primitive[String] = Json.Key.Primitive.syntax.string

    object constant:
      export Json.Key.Constant.syntax.constant as apply
      def apply(value: String): Json.Key.Constant[String] = apply(string, value)

    export Json.Key.Union.syntax.branch

    def enumeration[A, B](codec: => Json.Key.Primitive[A])(using mapping: Mapping[B, A]): Json.Key.Enumeration[B] =
      Json.Key.Enumeration.syntax.enumeration(codec, mapping)

    def enumeration[A: Order, B](codec: => Json.Key.Primitive[A])(f: B => A)(using
        EnumerationValues.Aux[B, B]
    ): Json.Key.Enumeration[B] = enumeration(codec)(using Mapping.enumeration(f))

  export Json.Record.syntax.field

  def field[A](name: String, codec: => Json[A]): Json.Record[A] = field(name, key = key.string, value = codec)

  export Json.Union.syntax.branch

  object collection:
    private val invariant = Json.Collection.syntax
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
    export Json.Constant.syntax.constant as apply
    def apply(value: String): Json.Constant[String] = apply(string, value)
    def apply(value: Int): Json.Constant[Int] = apply(int, value)
    def apply(value: Long): Json.Constant[Long] = apply(long, value)
    def apply(value: Float): Json.Constant[Float] = apply(float, value)
    def apply(value: Double): Json.Constant[Double] = apply(double, value)
    def apply(value: Boolean): Json.Constant[Boolean] = apply(boolean, value)

  object dictionary:
    private val invariant = Json.Dictionary.syntax
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

  def enumeration[A, B](codec: => Json.Primitive[A])(using mapping: Mapping[B, A]): Json.Enumeration[B] =
    Json.Enumeration.syntax.enumeration(codec, mapping)

  def enumeration[A: Order, B](codec: => Json.Primitive[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Json.Enumeration[B] = enumeration(codec)(using Mapping.enumeration(f))

object JsonDsl extends JsonDsl
