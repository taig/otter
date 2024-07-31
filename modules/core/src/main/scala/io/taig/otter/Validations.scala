package io.taig.otter

import java.util.regex.Pattern
import io.taig.otter.validation.Validations as Base
import scala.collection.immutable.Iterable
import cats.syntax.all.*
import cats.data.Chain
import cats.data.NonEmptyChain

trait Validations extends Types:
  val email: CodecValidation.Primitive[String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): CodecValidation.Primitive[String, Unit] =
    Base.matches(pattern).mapConstraint(Constraint.Primitive.Matches.apply).mapActual(Data.String.apply)

  object maxItems:
    def apply[A](reference: Long, count: A => Long): CodecValidation.Collection[A, Unit] =
      Base.maxItems(reference, count).mapConstraint(Constraint.Collection.MaxItems.apply).mapActual(Data.Number.apply)

    def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Collection[A, Unit] =
      maxItems(reference, _.size.toLong)

  object maxProperties:
    def apply[A](reference: Long, count: A => Long): CodecValidation.Object[A, Unit] = Base
      .maxProperties(reference, count)
      .mapConstraint(Constraint.Object.MaxProperties.apply)
      .mapActual(Data.Number.apply)

    def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Object[A, Unit] = maxProperties(reference, _.size)

  object minItems:
    def apply[A](reference: Long, count: A => Long): CodecValidation.Collection[A, Unit] =
      Base.minItems(reference, count).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

    def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Collection[A, Unit] = minItems(reference, _.size)

  object minProperties:
    def apply[A](reference: Long, count: A => Long): CodecValidation.Object[A, Unit] = Base
      .minProperties(reference, count)
      .mapConstraint(Constraint.Object.MaxProperties.apply)
      .mapActual(Data.Number.apply)

    def iterable[A <: Iterable[?]](reference: Long): CodecValidation.Object[A, Unit] = minProperties(reference, _.size)

  object nonEmpty:
    object collection:
      def apply[A, B](uncons: A => Option[(B, A)]): CodecValidation.Collection[A, (B, A)] =
        Base.nonEmpty[A, B](uncons).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

      def iterable[F[a] <: Iterable[a], A]: CodecValidation.Collection[F[A], (A, F[A])] = apply(_.uncons)

      def chain[A]: CodecValidation.Collection[Chain[A], NonEmptyChain[A]] =
        apply[Chain[A], A](_.uncons).map(NonEmptyChain.fromChainPrepend)

    object obj:
      def apply[A, B](uncons: A => Option[(B, A)]): CodecValidation.Object[A, (B, A)] =
        Base.nonEmpty[A, B](uncons).mapConstraint(Constraint.Object.MinProperties.apply).mapActual(Data.Number.apply)

      def iterable[F[a] <: Iterable[a], A]: CodecValidation.Object[F[A], (A, F[A])] = apply(_.uncons)

      def map[F[a, b] <: Map[a, b], A, B]: CodecValidation.Object[F[A, B], ((A, B), F[A, B])] =
        apply(fab => fab.headOption.map((_, fab.tail.asInstanceOf[F[A, B]])))

      def chain[A]: CodecValidation.Object[Chain[A], NonEmptyChain[A]] =
        apply[Chain[A], A](_.uncons).map(NonEmptyChain.fromChainPrepend)

  def uniqueItems[F[a] <: Iterable[a], A](codec: Codec[A]): CodecValidation.Collection[F[A], Unit] =
    Base.uniqueItems
      .mapConstraint(_ => Constraint.Collection.UniqueItems)
      .mapActual(as => codec.toCollection.encode(as.toList.toVector))
