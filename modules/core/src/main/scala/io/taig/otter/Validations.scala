package io.taig.otter

import java.util.regex.Pattern
import io.taig.otter.validation.Validation
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptySeq
import cats.data.Validated
import cats.syntax.all.*
import scala.util.chaining.*
import scala.collection.immutable.SortedSet
import cats.Order
import cats.implicits.*

trait Validations extends Schemas, Syntax:
  val email: SchemaValidation.Primitive[String, Nothing, String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): SchemaValidation.Primitive[String, Nothing, String, Unit] = Validation
    .when(Constraint.Primitive.Matches(pattern))((value: String) => pattern.matcher(value).matches())
    .mapActual((string, _))

  def maxItems[F[a] <: Iterable[a], A](reference: Long): SchemaValidation.Collection[F[A], Long, Unit] =
    Validation.validated(Constraint.Collection.MinItems(reference)): fa =>
      val length = fa.size
      Validated.condNec(length <= reference, (), (long, length.toLong))

  def minItems[F[a] <: Iterable[a], A](reference: Long): SchemaValidation.Collection[F[A], Long, Unit] =
    Validation.validated(Constraint.Collection.MinItems(reference)): fa =>
      val length = fa.size
      Validated.condNec(length >= reference, (), (long, length.toLong))

  def nonEmpty[F[a] <: Iterable[a] { def tail: F[a] }, A]: SchemaValidation.Collection[F[A], 0L, (A, F[A])] =
    Validation.validated(Constraint.Collection.MinItems(reference = 1)): fa =>
      fa.headOption.toValidNec[0L](0L).tupleRight(fa.tail).leftMap(_.tupleLeft(long))

  def uniqueItems[F[a] <: Iterable[a], A](
      writer: Schema.Writer[A]
  ): SchemaValidation.Collection[F[A], NonEmptyList[A], Unit] =
    Validation.validated(Constraint.Collection.UniqueItems):
      _.groupBy(identity)
        .collect { case (a, as) if as.sizeCompare(1) > 0 => a }
        .toList
        .pipe(NonEmptyList.fromList)
        .toInvalidNec(())
        .leftMap(_.tupleLeft(writer.collection.transform(nonEmptyList)))

  def vector[A]: Transformation.Plain[Vector[A], Vector[A]] = Transformation.ask

  def seq[A]: Transformation.Plain[Vector[A], Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)

  def nonEmptySeq[A]: SchemaTransformation.Collection[Vector[A], 0L, NonEmptySeq[A]] =
    seq[A].ivalidate(nonEmpty)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

  def list[A]: Transformation.Plain[Vector[A], List[A]] = vector[A].imap(_.toList)(_.toVector)

  def nonEmptyList[A]: SchemaTransformation.Collection[Vector[A], 0L, NonEmptyList[A]] =
    list[A].ivalidate(nonEmpty)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

  def set[A: Order](writer: Schema.Writer[A]): SchemaTransformation.Collection[Vector[A], NonEmptyList[A], Set[A]] =
    vector[A].validate_(uniqueItems(writer)).imap(_.to(Set))(_.toVector)

  def sortedSet[A: Order](
      writer: Schema.Writer[A]
  ): SchemaTransformation.Collection[Vector[A], NonEmptyList[A], SortedSet[A]] =
    set[A](writer).imap(SortedSet.from)(_.toSet)

  def nonEmptySet[A: Order](
      writer: Schema.Writer[A]
  ): SchemaTransformation.Collection[Vector[A], NonEmptyList[A] | Long, NonEmptySet[A]] = ???
  // sortedSet[A](writer).ivalidate(nonEmpty)({ case (a, as) => as + a }).imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))
