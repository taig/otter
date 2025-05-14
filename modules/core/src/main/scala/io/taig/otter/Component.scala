package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import cats.~>
import io.taig.otter.Argument
import io.taig.otter.Constraint
import io.taig.otter.Metadata
import io.taig.otter.Reference
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt
import scala.Int as SInt
import scala.Long as SLong

import scala.collection.immutable.SortedSet
import cats.arrow.FunctionK
import io.taig.otter.Shape
import java.util.UUID

object Component:
  trait Collection[+Self[_], -Value[_]](using self: Shape.Collection[Self, Value]):
    final def list[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[List[A]] = self.linked(schema, minimum = minimum.toOption, maximum = maximum.toOption, uniqueItems)

    final def vector[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Vector[A]] = self.indexed(schema, minimum = minimum.toOption, maximum = maximum.toOption, uniqueItems)

    final def nonEmptyList[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyList[A]] = list(schema, minimum = minimum.getOrElse(1).max(1), maximum, uniqueItems)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def nonEmptyVector[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyVector[A]] = vector(schema, minimum = minimum.getOrElse(1).max(1), maximum, uniqueItems)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Seq[A]] = vector(schema, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

    final def nonEmptySeq[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptySeq[A]] = nonEmptyVector(schema, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySeq(values.head, values.tail))(values =>
        NonEmptyVector(values.head, values.tail.toVector)
      )

    final def chain[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Chain[A]] = vector(schema, minimum, maximum, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    final def nonEmptyChain[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyChain[A]] = nonEmptyVector(schema, minimum, maximum, uniqueItems)
      .imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

    final def set[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Set[A]] = vector(schema, minimum, maximum, uniqueItems).imap(_.toSet)(_.toVector)

    final def sortedSet[A: Order](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[SortedSet[A]] = list(schema, minimum, maximum, uniqueItems).imap(SortedSet.from)(_.toList)

    final def nonEmptySet[A: Order](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptySet[A]] = nonEmptyList(schema, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)

  trait Field[Self[_], -Key[_], -Value[_], Record[_]](using
      shape: Shape.Field[Self, Key, Value]
      // record: Shape.Record[Record, Self]
  ):
    final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] =
      shape.field(name, key, value)

    extension [A](self: Self[A]) def toRecord: Record[A] = ??? // record(self)

  object Field:
    trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
        extends Component.Field.Primitive.Boolean[Self, Key, Value, Record],
          Component.Field.Primitive.Number[Self, Key, Value, Record],
          Component.Field.Primitive.String[Self, Key, Value, Record]:
      override def key: Component.Primitive[Key]

    object Primitive:
      trait Boolean[Self[_], Key[_], -Value[_], Record[_]] extends Component.Field[Self, Key, Value, Record]:
        def key: Component.Primitive.Boolean[Key]

        final def field[A](name: SBoolean, codec: => Value[A]): Self[A] =
          field(name, key = key.boolean, value = codec)

      trait Number[Self[_], Key[_], -Value[_], Record[_]] extends Component.Field[Self, Key, Value, Record]:
        def key: Component.Primitive.Number[Key]

        final def field[A](name: BigDecimal, codec: => Value[A]): Self[A] =
          field(name, key = key.bigDecimal, value = codec)
        final def field[A](name: BigInt, codec: => Value[A]): Self[A] = field(name, key = key.bigInteger, value = codec)
        final def field[A](name: JBigDecimal, codec: => Value[A]): Self[A] =
          field(name, key = key.jBigDecimal, value = codec)
        final def field[A](name: JBigInteger, codec: => Value[A]): Self[A] =
          field(name, key = key.jBigInteger, value = codec)
        final def field[A](name: SDouble, codec: => Value[A]): Self[A] = field(name, key = key.double, value = codec)
        final def field[A](name: SFloat, codec: => Value[A]): Self[A] = field(name, key = key.float, value = codec)
        final def field[A](name: SInt, codec: => Value[A]): Self[A] = field(name, key = key.int, value = codec)
        final def field[A](name: SLong, codec: => Value[A]): Self[A] = field(name, key = key.long, value = codec)

      trait String[Self[_], Key[_], -Value[_], Record[_]] extends Component.Field[Self, Key, Value, Record]:
        def key: Component.Primitive.String[Key]

        final def field[A](name: JString, codec: => Value[A]): Self[A] =
          field(name, key = key.string, value = codec)

  trait Primitive[+Self[_]]
      extends Component.Primitive.Boolean[Self],
        Component.Primitive.Number[Self],
        Component.Primitive.String[Self]

  object Primitive:
    trait Boolean[+Self[_]](using self: Shape.Primitive.Boolean[Self]):
      final val boolean: Self[SBoolean] = self.boolean

    trait Number[+Self[_]](using self: Shape.Primitive.Number[Self]):
      final def jBigDecimal(
          minimum: Argument[Comparison[JBigDecimal]] = Argument.Default,
          maximum: Argument[Comparison[JBigDecimal]] = Argument.Default,
          multiple: Argument[JBigDecimal] = Argument.Default
      ): Self[JBigDecimal] = self.jBigDecimal(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val jBigDecimal: Self[JBigDecimal] = jBigDecimal()

      final def bigDecimal(
          minimum: Argument[Comparison[SBigDecimal]] = Argument.Default,
          maximum: Argument[Comparison[SBigDecimal]] = Argument.Default,
          multiple: Argument[SBigDecimal] = Argument.Default
      ): Self[SBigDecimal] = jBigDecimal(
        minimum = minimum.map(_.map(_.bigDecimal)),
        maximum = maximum.map(_.map(_.bigDecimal)),
        multiple = multiple.map(_.bigDecimal)
      ).imap(SBigDecimal.apply)(_.bigDecimal)

      final val bigDecimal: Self[SBigDecimal] = bigDecimal()

      final def jBigInteger(
          minimum: Argument[Comparison[JBigInteger]] = Argument.Default,
          maximum: Argument[Comparison[JBigInteger]] = Argument.Default,
          multiple: Argument[JBigInteger] = Argument.Default
      ): Self[JBigInteger] = self.jBigInteger(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val jBigInteger: Self[JBigInteger] = jBigInteger()

      final def bigInteger(
          minimum: Argument[Comparison[SBigInt]] = Argument.Default,
          maximum: Argument[Comparison[SBigInt]] = Argument.Default,
          multiple: Argument[SBigInt] = Argument.Default
      ): Self[SBigInt] = jBigInteger(
        minimum = minimum.map(_.map(_.bigInteger)),
        maximum = maximum.map(_.map(_.bigInteger)),
        multiple = multiple.map(_.bigInteger)
      ).imap(SBigInt.apply)(_.bigInteger)

      final val bigInteger: Self[SBigInt] = bigInteger()

      final def double(
          minimum: Argument[Comparison[SDouble]] = Argument.Default,
          maximum: Argument[Comparison[SDouble]] = Argument.Default,
          multiple: Argument[SDouble] = Argument.Default
      ): Self[SDouble] = self.double(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val double: Self[SDouble] = double()

      final def float(
          minimum: Argument[Comparison[SFloat]] = Argument.Default,
          maximum: Argument[Comparison[SFloat]] = Argument.Default,
          multiple: Argument[SFloat] = Argument.Default
      ): Self[SFloat] = self.float(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val float: Self[SFloat] = float()

      final def int(
          minimum: Argument[Comparison[SInt]] = Argument.Default,
          maximum: Argument[Comparison[SInt]] = Argument.Default,
          multiple: Argument[SInt] = Argument.Default
      ): Self[SInt] = self.int(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val int: Self[SInt] = int()

      final def long(
          minimum: Argument[Comparison[SLong]] = Argument.Default,
          maximum: Argument[Comparison[SLong]] = Argument.Default,
          multiple: Argument[SLong] = Argument.Default
      ): Self[SLong] = self.long(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val long: Self[SLong] = long()

    trait String[+Self[_]](using self: Shape.Primitive.String[Self]):
      final def string(
          minimum: Argument[SInt] = Argument.Default,
          maximum: Argument[SInt] = Argument.Default,
          matches: Argument[Pattern] = Argument.Default
      ): Self[JString] = self.string(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        matches = matches.toOption
      )

      final val string: Self[JString] = string()

      implicit final class ToStringComponentExtension(self: string.type)
          extends StringComponentExtension[Self, JString]:
        override protected def empty: JString = ""
        override protected def isEmpty(a: JString): SBoolean = a.isEmpty

        def apply(
            minimum: Argument[Int] = Argument.Default,
            maximum: Argument[Int] = Argument.Default,
            matches: Argument[Pattern] = Argument.Default
        ): Self[JString] = string(minimum, maximum, matches)

      final def parser[A](
          name: JString,
          minimum: Argument[SInt] = Argument.Default,
          maximum: Argument[SInt] = Argument.Default,
          matches: Argument[Pattern] = Argument.Default
      )(f: JString => Either[JString, A])(g: A => JString): Self[A] = self.parser(
        name,
        decode = f,
        encode = g,
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        matches = matches.toOption
      )

      final val uuid: Self[UUID] = parser(name = "uuid") { value =>
        Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
      }(_.show)

      final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)
