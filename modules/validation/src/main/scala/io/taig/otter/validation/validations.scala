package io.taig.otter.validation

import java.util.regex.Pattern
import cats.data.Validated
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.util.chaining.*
import scala.collection.immutable.Iterable
import scala.Ordering.Implicits.*

trait Validations:
  def matches(pattern: Pattern): Validation[String, Pattern, String, Unit] =
    Validation.when(pattern)(pattern.matcher(_).matches())

  def maxItems[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size >= reference, (), size.toLong)

  def minItems[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size >= reference, (), size.toLong)

  def maxProperties[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size <= reference, (), size)

  def minProperties[A](reference: Long, count: A => Long): Validation[A, reference.type, Long, Unit] =
    Validation.validated(reference): a =>
      val size = count(a)
      Validated.condNec(size >= reference, (), size)

  def maximum[A: Numeric](reference: A, exclusive: Boolean): Validation[A, reference.type, A, Unit] =
    Validation.when(reference): value =>
      if exclusive then value < reference else value <= reference

  def minimum[A: Numeric](reference: A, exclusive: Boolean): Validation[A, reference.type, A, Unit] =
    Validation.when(reference): value =>
      if exclusive then value > reference else value >= reference

  def nonEmpty[A, B](uncons: A => Option[(B, A)]): Validation[A, 1L, 0L, (B, A)] =
    Validation.validated(1L)(fa => uncons(fa).toValidNec[0L](0L))

  def uniqueItems[F[a] <: Iterable[a], A]: Validation[F[A], "uniqueItems", NonEmptyList[A], Unit] =
    Validation.validated("uniqueItems"):
      _.groupBy(identity)
        .collect { case (a, as) if as.sizeCompare(1) > 0 => a }
        .toList
        .pipe(NonEmptyList.fromList)
        .toInvalidNec(())

object Validations extends Validations
