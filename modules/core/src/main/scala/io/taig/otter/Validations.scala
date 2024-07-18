package io.taig.otter

import java.util.regex.Pattern
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import scala.collection.immutable.SortedSet
import cats.Order
import cats.implicits.*
import io.taig.otter.validation.Validations as Base

trait Validations extends Codecs:
  val email: CodecValidation.Primitive[String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): CodecValidation.Primitive[String, Unit] =
    Base.matches(pattern).mapConstraint(Constraint.Primitive.Matches.apply).mapActual(Data.String.apply)

  def maxItems[F[a] <: Iterable[a], A](reference: Long): CodecValidation.Collection[F[A], Unit] =
    Base.maxItems(reference).mapConstraint(Constraint.Collection.MaxItems.apply).mapActual(Data.Number.apply)

  def minItems[F[a] <: Iterable[a], A](reference: Long): CodecValidation.Collection[F[A], Unit] =
    Base.minItems(reference).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  def nonEmpty[F[a] <: Iterable[a] { def tail: F[a] }, A]: CodecValidation.Collection[F[A], (A, F[A])] =
    Base.nonEmpty[F, A].mapConstraint(Constraint.Collection.MinItems.apply).mapActual(Data.Number.apply)

  def uniqueItems[F[a] <: Iterable[a], A](using
      codec: Codec[?, A]
  ): CodecValidation.Collection[F[A], Unit] = Base.uniqueItems
    .mapConstraint(_ => Constraint.Collection.UniqueItems)
    .mapActual(codec.collection.apply(nonEmptyList).encode)

  def vector[A]: Transformation.Plain[Vector[A], Vector[A]] = Transformation.ask

  def seq[A]: Transformation.Plain[Vector[A], Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)

  def nonEmptySeq[A]: CodecTransformation.Collection[Vector[A], NonEmptySeq[A]] =
    seq[A].ivalidate(nonEmpty)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

  def list[A]: Transformation.Plain[Vector[A], List[A]] = vector[A].imap(_.toList)(_.toVector)

  def nonEmptyList[A]: CodecTransformation.Collection[Vector[A], NonEmptyList[A]] =
    list[A].ivalidate(nonEmpty)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

  def set[A: Codec[?, *]: Order]: CodecTransformation.Collection[Vector[A], Set[A]] =
    vector[A].ivalidate_(uniqueItems).imap(_.to(Set))(_.toVector)

  def sortedSet[A: Codec[?, *]: Order]: CodecTransformation.Collection[Vector[A], SortedSet[A]] =
    set[A].imap(SortedSet.from)(_.toSet)

  def nonEmptySet[A: Codec[?, *]: Order]: CodecTransformation.Collection[Vector[A], NonEmptySet[A]] = sortedSet[A]
    .ivalidate(nonEmpty)({ case (a, as) => as + a })
    .imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))
