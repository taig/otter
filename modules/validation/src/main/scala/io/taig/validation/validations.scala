package io.taig.validation

import cats.data.Chain
import cats.syntax.all.*
import cats.{Applicative, Eq, Foldable, Monoid, MonoidK, Semigroup, SemigroupK, Traverse, UnorderedFoldable}

import java.util.UUID
import scala.Numeric.Implicits.*
import scala.Ordering.Implicits.*
import scala.collection.IterableOps
import scala.collection.immutable.SortedMap
import scala.util.matching.Regex

object validations:
  def refine[In, Out](tpe: String)(f: In => Option[Out]): Validation[String, In, In, Out] =
    Validation.fromOptionNec(Constraint(s"type.$tpe", reference = tpe.some))(f)

//  abstract class collection[F[_]]:
//    protected def size[A](fa: F[A]): Long
//    protected def contains[A: Eq](fa: F[A], a: A): Boolean
//    protected def uncons[A](fa: F[A]): Option[(A, F[A])]
//
//    final def size[A]: Validation[Nothing, Nothing, F[A], Long] = Validation.fromFunction(size)
//
//    final def atLeast[A](reference: Long): Validation[Long, F[A], F[A], Unit] =
//      size[A]
//        .andThen(numeric.lessThan(reference, equal = true))
//        .withConstraint(identifiers.collection.atLeast.toConstraint(_))
//        .reset
//
//    final def atMost[A](reference: Long): Validation[Long, F[A], F[A], Unit] =
//      size[A]
//        .andThen(numeric.lessThan(reference, equal = true))
//        .withConstraint(identifiers.collection.atMost.toConstraint(_))
//        .reset
//
//    final def contains[A: Eq](a: A): Validation[A, F[A], F[A], Unit] =
//      Validation.condNec(identifiers.collection.contains.toConstraint(reference = a.some))(contains(_, a))
//
//    def nonEmpty[A]: Validation[Nothing, F[A], F[A], (A, F[A])] =
//      Validation.fromOptionNec(identifiers.collection.nonEmpty.toConstraint(reference = none))(uncons)
//
//  object collection:
//    class seq[F[a] <: IterableOps[a, F, F[a]]] extends collection[F]:
//      override def size[A](fa: F[A]): Long = fa.size.toLong
//      override def contains[A: Eq](fa: F[A], a: A): Boolean = fa.exists(_ === a)
//      override def uncons[A](fa: F[A]): Option[(A, F[A])] = fa.headOption.tupleRight(fa.tail)
//
//    val list: collection[List] = new seq[List]
//    val seq: collection[Seq] = new seq[Seq]
//    val vector: collection[Vector] = new seq[Vector]
//    val chain: collection[Chain] = new collection[Chain]:
//      override def size[A](fa: Chain[A]): Long = fa.length
//      override def contains[A: Eq](fa: Chain[A], a: A): Boolean = fa.contains(a)
//      override def uncons[A](fa: Chain[A]): Option[(A, Chain[A])] = fa.uncons
//
//  class map[F[a, b] <: Map[a, b]]:
//    def nonEmpty[A, B]: Validation[Nothing, F[A, B], F[A, B], ((A, B), Map[A, B])] =
//      Validation.fromOptionNec(identifiers.collection.nonEmpty.toConstraint(reference = none)) { fa =>
//        fa.headOption.tupleRight(fa.tail)
//      }
//
//  object map:
//    val default: map[Map] = new map[Map]
//    val sorted: map[SortedMap] = new map[SortedMap]

  object numeric:
    def greaterThan[In: Numeric](
        reference: In,
        equal: Boolean = false,
        delta: Option[In] = none
    ): Validation[In, In, In, Unit] =
      // TODO ref obj with equal & delta
      Validation.condNec(Constraint("numeric.greaterThan", reference.some)): input =>
        delta match
          case Some(delta) if equal => input - reference >= -delta
          case Some(delta)          => input - reference > -delta
          case None if equal        => input >= reference
          case None                 => input > reference

//    def lessThan[In: Numeric](
//        reference: In,
//        equal: Boolean = false,
//        delta: Option[In] = none
//    ): Validation[In, In, In, Unit] =
//      Validation.condNec(identifiers.numeric.lessThan.toConstraint(reference.some, equal, delta)) { input =>
//        delta match
//          case Some(delta) if equal => reference - input >= -delta
//          case Some(delta)          => reference - input > -delta
//          case None if equal        => input <= reference
//          case None                 => input < reference
//      }
//
//    def equal[In: Numeric](reference: In, delta: Option[In] = none): Validation[In, In, In, Unit] =
//      (greaterThan(reference, equal = true, delta) *> lessThan(reference, equal = true, delta))
//        .withConstraint(_ => identifiers.numeric.equal.toConstraint(reference.some))
//
//  object parser:
//    def apply[A](name: String)(f: String => Option[A]): Validation[String, String, String, A] =
//      Validation.fromOptionNec(identifiers.parser(name.toLowerCase).toConstraint(name.some))(f)
//
//    val uuid: Validation[String, String, String, UUID] = parser("UUID") { value =>
//      try Some(UUID.fromString(value))
//      catch { case _: IllegalArgumentException => None }
//    }
//
  object text:
    val length: Validation[Nothing, Nothing, String, Int] = Validation.lift(_.length)
    val trim: Validation[Nothing, Nothing, String, String] = Validation.lift(_.trim)

//    def atLeast(reference: Int): Validation[Int, String, String, Unit] =
//      length
//        .andThen(numeric.greaterThan(reference, equal = true))
//        .withConstraint(identifiers.text.atLeast.toConstraint(_))
//        .reset
//
//    def atMost(reference: Int): Validation[Int, String, String, Unit] =
//      length
//        .andThen(numeric.lessThan(reference, equal = true))
//        .withConstraint(identifiers.text.atMost.toConstraint(_))
//        .reset
//
//    val nonEmpty: Validation[Int, String, String, Unit] = atLeast(reference = 1)
//
//    val email: Validation[Nothing, String, String, Unit] = matches("""^.+@.+$""".r)
//      .withConstraint(_ => identifiers.text.email.toConstraint(none))
//
//    val empty: Validation[Int, String, String, Unit] = atMost(reference = 0)
//
//    def equal(reference: String): Validation[String, String, String, Unit] =
//      Validation.condNec(identifiers.text.equal.toConstraint(reference.some))(_ === reference)
//
//    def exactly(reference: Int): Validation[Int, String, String, Unit] = length
//      .andThen(numeric.equal(reference))
//      .withConstraint(identifiers.text.exactly.toConstraint(_))
//      .reset
//
//    def matches(regex: Regex): Validation[Regex, String, String, Unit] =
//      Validation.condNec(identifiers.text.matches.toConstraint(regex.some))(regex.matches)
//
//    val required: Validation[Nothing, String, String, String] = trim
//      .andThen(nonEmpty.tap)
//      .withConstraint(_ => identifiers.text.required.toConstraint(none))
//      .reset
