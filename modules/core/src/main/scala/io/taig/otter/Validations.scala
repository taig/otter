package io.taig.otter

import java.util.regex.Pattern
import io.taig.otter.validation.Validations as Base
import scala.collection.immutable.Iterable
import cats.syntax.all.*
import cats.data.Chain
import cats.data.NonEmptyChain
import scala.Numeric.Implicits.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

trait Validations extends Types:
  val email: CodecValidation[Data.Primitive, String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): CodecValidation[Data.Primitive, String, Unit] =
    Base.matches(pattern).mapConstraint(Constraint.Primitive.Matches.apply).mapActual(Data.String.apply)

  def matches(reference: String): CodecValidation[Data.Primitive, String, Unit] =
    matches(Pattern.compile(Pattern.quote(reference)))

  // object maxItems:
  //   def apply[A](reference: Long, count: A => Long): CodecValidation.Array[A, Unit] =
  //     Base.maxItems(reference, count).mapConstraint(Constraint.Collection.MaxItems.apply).mapActual(Data.Number.apply)

  //   def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Array[A, Unit] =
  //     maxItems(reference, _.size.toLong)

  // object minItems:
  //   def apply[A](reference: Long, count: A => Long): CodecValidation.Array[A, Unit] =
  //     Base.minItems(reference, count).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  //   def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Array[A, Unit] = minItems(reference, _.size)

  // object maxLength:
  //   def apply[A](reference: Int, count: A => Int): CodecValidation.Primitive[A, Unit] =
  //     Base.maxLength(reference, count).mapConstraint(Constraint.Primitive.MaxLength.apply).mapActual(Data.Number.apply)

  //   def apply(reference: Int): CodecValidation.Primitive[CharSequence, Unit] = maxLength(reference, _.length)

  // object minLength:
  //   def apply[A](reference: Int, count: A => Int): CodecValidation.Primitive[A, Unit] =
  //     Base.minLength(reference, count).mapConstraint(Constraint.Primitive.MinLength.apply).mapActual(Data.Number.apply)

  //   def apply(reference: Int): CodecValidation.Primitive[CharSequence, Unit] = minLength(reference, _.length)

  // object length:
  //   def apply[A](reference: Int, count: A => Int): CodecValidation.Primitive[A, Unit] =
  //     minLength(reference, count) *> maxLength(reference, count)

  //   def apply(reference: Int): CodecValidation.Primitive[CharSequence, Unit] = length(reference, _.length)

  // object maxProperties:
  //   def apply[A](reference: Long, count: A => Long): CodecValidation.Object[A, Unit] = Base
  //     .maxProperties(reference, count)
  //     .mapConstraint(Constraint.Object.MaxProperties.apply)
  //     .mapActual(Data.Number.apply)

  //   def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Object[A, Unit] = maxProperties(reference, _.size)

  // object minProperties:
  //   def apply[A](reference: Long, count: A => Long): CodecValidation.Object[A, Unit] = Base
  //     .minProperties(reference, count)
  //     .mapConstraint(Constraint.Object.MaxProperties.apply)
  //     .mapActual(Data.Number.apply)

  //   def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Object[A, Unit] = minProperties(reference, _.size)

  // def maximum[A <: Matchable: Numeric](reference: A, exclusive: Boolean = false): CodecValidation.Primitive[A, Unit] =
  //   Base
  //     .maximum(reference, exclusive)
  //     .mapConstraint(value => Constraint.Primitive.Maximum(toNumber(value), exclusive))
  //     .mapActual(toNumber)

  // def minimum[A <: Matchable: Numeric](reference: A, exclusive: Boolean = false): CodecValidation.Primitive[A, Unit] =
  //   Base
  //     .minimum(reference, exclusive)
  //     .mapConstraint(value => Constraint.Primitive.Minimum(toNumber(value), exclusive))
  //     .mapActual(toNumber)

  // object nonEmpty:
  //   object collection:
  //     def apply[A, B](uncons: A => Option[(B, A)]): CodecValidation.Array[A, (B, A)] =
  //       Base.nonEmpty[A, B](uncons).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  //     def iterable[F[a] <: Iterable[a], A]: CodecValidation.Array[F[A], (A, F[A])] = apply(_.uncons)

  //     def chain[A]: CodecValidation.Array[Chain[A], NonEmptyChain[A]] =
  //       apply[Chain[A], A](_.uncons).map(NonEmptyChain.fromChainPrepend)

  //   object obj:
  //     def apply[A, B](uncons: A => Option[(B, A)]): CodecValidation.Object[A, (B, A)] =
  //       Base.nonEmpty[A, B](uncons).mapConstraint(Constraint.Object.MinProperties.apply).mapActual(Data.Number.apply)

  //     def iterable[F[a] <: Iterable[a], A]: CodecValidation.Object[F[A], (A, F[A])] = apply(_.uncons)

  //     def map[F[a, b] <: Map[a, b], A, B]: CodecValidation.Object[F[A, B], ((A, B), F[A, B])] =
  //       apply(fab => fab.headOption.map((_, fab.tail.asInstanceOf[F[A, B]])))

  //     def chain[A]: CodecValidation.Object[Chain[A], NonEmptyChain[A]] =
  //       apply[Chain[A], A](_.uncons).map(NonEmptyChain.fromChainPrepend)

  // def parse[A](name: String)(f: String => Option[A]): CodecValidation.Any[String, A] =
  //   Base.parse[A](name)(f).mapConstraint(Constraint.Type.apply).mapActual(Data.String.apply)

  // def uniqueItems[F[a] <: Iterable[a], A](codec: Codec[A]): CodecValidation.Array[F[A], Unit] =
  //   Base.uniqueItems
  //     .mapConstraint(_ => Constraint.Collection.UniqueItems)
  //     .mapActual(as => codec.toCollection.encode(as.toList.toVector))

  // private def toNumber[A <: Matchable: Numeric](a: A): Data.Number = a match
  //   case value: Int         => Data.Number(value)
  //   case value: Long        => Data.Number(value)
  //   case value: Double      => Data.Number(value)
  //   case value: Float       => Data.Number(value)
  //   case value: JBigDecimal => Data.Number(value)
  //   case value: JBigInteger => Data.Number(value)
  //   case value: BigDecimal  => Data.Number(value.bigDecimal)
  //   case value: BigInt      => Data.Number(value.bigInteger)
  //   case value              => Data.Number(value.toDouble)

object Validations extends Validations
