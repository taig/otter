package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyMap
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import cats.~>
import io.taig.otter.Argument
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
import java.util.UUID
import cats.kernel.Eq
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues
import scala.collection.immutable.SortedMap

object Component:
  trait Branch[Self[_], -Key[_], -Value[_], Sum[_]](using shape: Shape.Branch[Self, Key, Value]):
    final def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = shape.branch(name, key, value)

    extension [A](self: Self[A]) def toSum: Sum[A] = ???

  object Branch:
    trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
        extends Component.Branch.Primitive.Boolean[Self, Key, Value, Record],
          Component.Branch.Primitive.Number[Self, Key, Value, Record],
          Component.Branch.Primitive.String[Self, Key, Value, Record]:
      override def key: Component.Primitive[Key]

    object Primitive:
      trait Boolean[Self[_], Key[_], -Value[_], Sum[_]] extends Component.Branch[Self, Key, Value, Sum]:
        def key: Component.Primitive.Boolean[Key]

        final def branch[A](name: SBoolean, schema: => Value[A]): Self[A] =
          branch(name, key = key.boolean, value = schema)

      trait Number[Self[_], Key[_], -Value[_], Sum[_]] extends Component.Branch[Self, Key, Value, Sum]:
        def key: Component.Primitive.Number[Key]

        final def branch[A](name: BigDecimal, schema: => Value[A]): Self[A] =
          branch(name, key = key.bigDecimal, value = schema)
        final def branch[A](name: BigInt, schema: => Value[A]): Self[A] =
          branch(name, key = key.bigInteger, value = schema)
        final def branch[A](name: JBigDecimal, schema: => Value[A]): Self[A] =
          branch(name, key = key.jBigDecimal, value = schema)
        final def branch[A](name: JBigInteger, schema: => Value[A]): Self[A] =
          branch(name, key = key.jBigInteger, value = schema)
        final def branch[A](name: SDouble, schema: => Value[A]): Self[A] =
          branch(name, key = key.double, value = schema)
        final def branch[A](name: SFloat, schema: => Value[A]): Self[A] = branch(name, key = key.float, value = schema)
        final def branch[A](name: SInt, schema: => Value[A]): Self[A] = branch(name, key = key.int, value = schema)
        final def branch[A](name: SLong, schema: => Value[A]): Self[A] = branch(name, key = key.long, value = schema)

      trait String[Self[_], Key[_], -Value[_], Sum[_]] extends Component.Branch[Self, Key, Value, Sum]:
        def key: Component.Primitive.String[Key]

        final def branch[A](name: JString, schema: => Value[A]): Self[A] =
          branch(name, key = key.string, value = schema)

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

  trait Constant[+Self[_], -Value[_]](using self: Shape.Constant[Self, Value]):
    final def constant[A: Eq](schema: => Value[A], value: A): Self[Unit] = self.constant(schema, value)

  object Constant:
    trait Primitive[+Self[_], -Value[_]]
        extends Constant.Primitive.Boolean[Self, Value],
          Constant.Primitive.Number[Self, Value],
          Constant.Primitive.String[Self, Value]:
      this: Component.Primitive[Value] =>

    object Primitive:
      trait Boolean[+Self[_], -Value[_]] extends Constant[Self, Value]:
        this: Component.Primitive.Boolean[Value] =>

        final def constant(value: SBoolean): Self[Unit] = constant(schema = boolean, value)

      trait Number[+Self[_], -Value[_]] extends Constant[Self, Value]:
        this: Component.Primitive.Number[Value] =>
        final def constant(value: JBigDecimal): Self[Unit] =
          constant(schema = jBigDecimal, value)(using Eq.fromUniversalEquals)
        final def constant(value: BigDecimal): Self[Unit] = constant(schema = bigDecimal, value)
        final def constant(value: JBigInteger): Self[Unit] =
          constant(schema = jBigInteger, value)(using Eq.fromUniversalEquals)
        final def constant(value: BigInt): Self[Unit] = constant(schema = bigInteger, value)
        final def constant(value: SLong): Self[Unit] = constant(schema = long, value)
        final def constant(value: SDouble): Self[Unit] = constant(schema = double, value)
        final def constant(value: SFloat): Self[Unit] = constant(schema = float, value)
        final def constant(value: SInt): Self[Unit] = constant(schema = int, value)

      trait String[+Self[_], -Value[_]] extends Constant[Self, Value]:
        this: Component.Primitive.String[Value] =>
        final def constant(value: JString): Self[Unit] = constant(schema = string, value)
        final def constant(value: UUID): Self[Unit] = constant(schema = uuid, value)

  trait DictionaryDsl[+Self[_], -Key[_], -Value[_]](using self: Shape.Dictionary[Self, Key, Value]):
    final def list[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[List[(A, B)]] = self.dictionary(key, value, minimum = minimum.toOption, maximum = maximum.toOption)

    final def nonEmptyList[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def vector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

    final def nonEmptyVector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

    final def nonEmptySeq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

    final def chain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

    final def nonEmptyChain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    final def map[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

    final def sortedMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

    final def nonEmptyMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)

  trait Enumeration[+Self[_], -Value[_]](using self: Shape.Enumeration[Self, Value]):
    final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
      self.enumeration(codec, mapping)

    final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
      enumeration(codec)(using Mapping.enumeration(f))

  trait Field[Self[_], -Key[_], -Value[_], Record[_]](using
      shape: Shape.Field[Self, Key, Value],
      record: Shape.Record[Record, Self]
  ):
    final def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] =
      shape.field(name, key, value)

    extension [A](self: Self[A]) def toRecord: Record[A] = record.record(self)

  object Field:
    trait Primitive[Self[_], Key[_], -Value[_], Record[_]]
        extends Component.Field.Primitive.Boolean[Self, Key, Value, Record],
          Component.Field.Primitive.Number[Self, Key, Value, Record],
          Component.Field.Primitive.String[Self, Key, Value, Record]:
      override def key: Component.Primitive[Key]

    object Primitive:
      trait Boolean[Self[_], Key[_], -Value[_], Record[_]] extends Component.Field[Self, Key, Value, Record]:
        def key: Component.Primitive.Boolean[Key]

        final def field[A](name: SBoolean, schema: => Value[A]): Self[A] =
          field(name, key = key.boolean, value = schema)

      trait Number[Self[_], Key[_], -Value[_], Record[_]] extends Component.Field[Self, Key, Value, Record]:
        def key: Component.Primitive.Number[Key]

        final def field[A](name: BigDecimal, schema: => Value[A]): Self[A] =
          field(name, key = key.bigDecimal, value = schema)
        final def field[A](name: BigInt, schema: => Value[A]): Self[A] =
          field(name, key = key.bigInteger, value = schema)
        final def field[A](name: JBigDecimal, schema: => Value[A]): Self[A] =
          field(name, key = key.jBigDecimal, value = schema)
        final def field[A](name: JBigInteger, schema: => Value[A]): Self[A] =
          field(name, key = key.jBigInteger, value = schema)
        final def field[A](name: SDouble, schema: => Value[A]): Self[A] = field(name, key = key.double, value = schema)
        final def field[A](name: SFloat, schema: => Value[A]): Self[A] = field(name, key = key.float, value = schema)
        final def field[A](name: SInt, schema: => Value[A]): Self[A] = field(name, key = key.int, value = schema)
        final def field[A](name: SLong, schema: => Value[A]): Self[A] = field(name, key = key.long, value = schema)

      trait String[Self[_], Key[_], -Value[_], Record[_]] extends Component.Field[Self, Key, Value, Record]:
        def key: Component.Primitive.String[Key]

        final def field[A](name: JString, schema: => Value[A]): Self[A] =
          field(name, key = key.string, value = schema)

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
