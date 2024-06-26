package io.taig.otter

import java.util.regex.Pattern
import io.taig.otter.validation.Validation
import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.all.*
import scala.util.chaining.*

trait Validations:
  val email: Validation[String, Constraint.Primitive[Nothing], String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): Validation[String, Constraint.Primitive[Nothing], String, Unit] =
    Validation.when(Constraint.Primitive.Matches(pattern))(pattern.matcher(_).matches())

  def maxItems[F[a] <: Iterable[a], A](reference: Long): Validation[F[A], Constraint.Collection, Long, Unit] =
    Validation.validated(Constraint.Collection.MinItems(reference)): fa =>
      val length = fa.size
      Validated.condNec(length <= reference, (), length.toLong)

  def minItems[F[a] <: Iterable[a], A](reference: Long): Validation[F[A], Constraint.Collection, Long, Unit] =
    Validation.validated(Constraint.Collection.MinItems(reference)): fa =>
      val length = fa.size
      Validated.condNec(length >= reference, (), length.toLong)

  def nonEmpty[F[a] <: Iterable[a] { def tail: F[a] }, A]: Validation[F[A], Constraint.Collection, 0L, (A, F[A])] =
    Validation.validated(Constraint.Collection.MinItems(reference = 1)): fa =>
      fa.headOption.toValidNec[0L](0L).tupleRight(fa.tail)

  def uniqueItems[F[a] <: Iterable[a], A]: Validation[F[A], Constraint.Collection, NonEmptyList[A], Unit] =
    Validation.validated(Constraint.Collection.UniqueItems):
      _.groupBy(identity)
        .collect { case (a, as) if as.sizeCompare(1) > 0 => a }
        .toList
        .pipe(NonEmptyList.fromList)
        .toInvalidNec(())
