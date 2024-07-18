package io.taig.otter

import java.util.regex.Pattern
import io.taig.otter.validation.Validations as Base
import scala.collection.immutable.Iterable
import cats.data.NonEmptyChain
import cats.data.Chain

trait Validations extends Types:
  val email: CodecValidation.Primitive[String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): CodecValidation.Primitive[String, Unit] =
    Base.matches(pattern).mapConstraint(Constraint.Primitive.Matches.apply).mapActual(Data.String.apply)

  def maxItems[F[a] <: Iterable[a], A](reference: Long): CodecValidation.Collection[F[A], Unit] =
    Base.maxItems(reference).mapConstraint(Constraint.Collection.MaxItems.apply).mapActual(Data.Number.apply)

  def minItems[F[a] <: Iterable[a], A](reference: Long): CodecValidation.Collection[F[A], Unit] =
    Base.minItems(reference).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  def nonEmpty[F[a] <: Iterable[a], A]: CodecValidation.Collection[F[A], (A, F[A])] =
    Base.nonEmpty[F, A].mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  def nonEmptyChain[A]: CodecValidation.Collection[Chain[A], NonEmptyChain[A]] =
    Base.nonEmptyChain[A].mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  def uniqueItems[F[a] <: Iterable[a], A](codec: Codec[A]): CodecValidation.Collection[F[A], Unit] =
    Base.uniqueItems
      .mapConstraint(_ => Constraint.Collection.UniqueItems)
      .mapActual(as => codec.toCollection.encode(as.toList.toVector))
