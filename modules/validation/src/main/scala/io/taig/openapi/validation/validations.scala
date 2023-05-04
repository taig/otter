package io.taig.openapi.validation

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*

import java.util.UUID
import scala.Numeric.Implicits.*
import scala.Ordering.Implicits.*
import scala.collection.IterableOps
import scala.collection.immutable.{SeqMap, SortedMap}
import scala.util.matching.Regex

object validations:
  def refine[In, Out](tpe: String)(f: In => Option[Out]): Validation[String, In, In, Out] =
    Validation.fromOptionNec(Constraint.tpe(tpe))(f)

  abstract class collection[F[_]]:
    protected def size[A](fa: F[A]): Long
    protected def isEmpty[A](fa: F[A]): Boolean
    protected def contains[A: Eq](fa: F[A], a: A): Boolean
    protected def uncons[A](fa: F[A]): Option[(A, F[A])]

    final def size[A]: Validation[Nothing, Nothing, F[A], Long] = Validation.lift(size)

    final def atLeast[A](reference: Long): Validation[Long, Long, F[A], Unit] =
      size[A]
        .andThen(numeric.greaterThan(reference, equal = true))
        .withConstraint(_ => Constraint.collection.atLeast(reference))

    final def atMost[A](reference: Long): Validation[Long, Long, F[A], Unit] =
      size[A]
        .andThen(numeric.lessThan(reference, equal = true))
        .withConstraint(_ => Constraint.collection.atMost(reference))

    final def contains[A: Eq](reference: A): Validation[A, F[A], F[A], Unit] =
      Validation.condNec(Constraint.collection.contains(reference))(contains(_, reference))

    final def empty[A]: Validation[Nothing, F[A], F[A], Unit] =
      Validation.condNec(Constraint.collection.empty)(isEmpty)

    final def exactly[A](reference: Long): Validation[Long, Long, F[A], Unit] =
      size[A]
        .andThen(numeric.equal(reference))
        .withConstraint(_ => Constraint.collection.exactly(reference))

    final def nonEmpty[A]: Validation[Nothing, F[A], F[A], (A, F[A])] =
      Validation.fromOptionNec(Constraint.collection.nonEmpty)(uncons)

  object collection:
    class seq[F[a] <: IterableOps[a, F, F[a]]] extends collection[F]:
      override def size[A](fa: F[A]): Long = fa.size.toLong
      override def isEmpty[A](fa: F[A]): Boolean = fa.isEmpty
      override def contains[A: Eq](fa: F[A], a: A): Boolean = fa.exists(_ === a)
      override def uncons[A](fa: F[A]): Option[(A, F[A])] = fa.headOption.map(head => (head, fa.tail))

    val list: collection[List] = new seq[List]
    val seq: collection[Seq] = new seq[Seq]
    val vector: collection[Vector] = new seq[Vector]
    val chain: collection[Chain] = new collection[Chain]:
      override def size[A](fa: Chain[A]): Long = fa.length
      override def isEmpty[A](fa: Chain[A]): Boolean = fa.isEmpty
      override def contains[A: Eq](fa: Chain[A], a: A): Boolean = fa.contains(a)
      override def uncons[A](fa: Chain[A]): Option[(A, Chain[A])] = fa.uncons

  class map[F[a, b] <: Map[a, b]]:
    def nonEmpty[A, B]: Validation[Nothing, F[A, B], F[A, B], ((A, B), Map[A, B])] =
      Validation.fromOptionNec(Constraint.collection.nonEmpty): fa =>
        fa.headOption.tupleRight(fa.tail)

  object map:
    val default: map[Map] = new map[Map]
    val sorted: map[SortedMap] = new map[SortedMap]
    val seq: map[SeqMap] = new map[SeqMap]

  object numeric:
    def greaterThan[In: Numeric](
        comparison: NumericComparison[In]
    ): Validation[NumericComparison[In], In, In, Unit] =
      Validation.condNec(Constraint.numeric.greaterThan(comparison)): input =>
        comparison.delta match
          case Some(delta) if comparison.equal => input - comparison.reference >= -delta
          case Some(delta)                     => input - comparison.reference > -delta
          case None if comparison.equal        => input >= comparison.reference
          case None                            => input > comparison.reference

    def greaterThan[In: Numeric](
        reference: In,
        equal: Boolean = false,
        delta: Option[In] = none
    ): Validation[NumericComparison[In], In, In, Unit] = greaterThan(NumericComparison(reference, equal, delta))

    def lessThan[In: Numeric](
        comparison: NumericComparison[In]
    ): Validation[NumericComparison[In], In, In, Unit] =
      Validation.condNec(Constraint.numeric.lessThan(comparison)): input =>
        comparison.delta match
          case Some(delta) if comparison.equal => comparison.reference - input >= -delta
          case Some(delta)                     => comparison.reference - input > -delta
          case None if comparison.equal        => input <= comparison.reference
          case None                            => input < comparison.reference

    def lessThan[In: Numeric](
        reference: In,
        equal: Boolean = false,
        delta: Option[In] = none
    ): Validation[NumericComparison[In], In, In, Unit] = lessThan(NumericComparison(reference, equal, delta))

    def equal[In: Numeric](reference: In, delta: Option[In] = none): Validation[NumericComparison[In], In, In, Unit] =
      (greaterThan(reference, equal = true, delta) *> lessThan(reference, equal = true, delta)).withConstraint: _ =>
        Constraint.numeric.equal(NumericComparison(reference, equal = true, delta))

  object parser:
    def apply[A](name: String)(f: String => Option[A]): Validation[String, String, String, A] =
      Validation.fromOptionNec(Constraint.parser(name))(f)

    val uuid: Validation[String, String, String, UUID] = parser("UUID"): value =>
      try Some(UUID.fromString(value))
      catch { case _: IllegalArgumentException => None }

  object text:
    val length: Validation[Nothing, Nothing, String, Int] = Validation.lift(_.length)
    val trim: Validation[Nothing, Nothing, String, String] = Validation.lift(_.trim)

    def atLeast(reference: Int): Validation[Int, Int, String, Unit] =
      length
        .andThen(numeric.greaterThan(reference, equal = true))
        .withConstraint(_ => Constraint.text.atLeast(reference))

    def atMost(reference: Int): Validation[Int, Int, String, Unit] =
      length
        .andThen(numeric.lessThan(reference, equal = true))
        .withConstraint(_ => Constraint.text.atMost(reference))

    val nonEmpty: Validation[Nothing, String, String, Unit] = atLeast(reference = 1)
      .withConstraint(_ => Constraint.required)
      .reset

    val email: Validation[Nothing, String, String, Unit] = matches("""^.+@.+$""".r)
      .withConstraint(_ => Constraint.text.email)

    val empty: Validation[Int, Int, String, Unit] = atMost(reference = 0)

    def equal(reference: String): Validation[String, String, String, Unit] =
      Validation.condNec(Constraint.text.equal(reference))(_ === reference)

    def exactly(reference: Int): Validation[Int, Int, String, Unit] = length
      .andThen(numeric.equal(reference))
      .withConstraint(_ => Constraint.text.exactly(reference))

    def matches(regex: Regex): Validation[Regex, String, String, Unit] =
      Validation.condNec(Constraint.text.matches(regex))(regex.matches)

    val required: Validation[Nothing, String, String, String] = trim.andThen(nonEmpty.tap)
