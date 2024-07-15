package io.taig.otter

import java.util.regex.Pattern
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import scala.collection.immutable.SortedSet
import cats.Order
import cats.implicits.*
import io.taig.otter.validation.Validations as Base

trait Validations extends Schemas:
  val email: SchemaValidation.Primitive[String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): SchemaValidation.Primitive[String, Unit] =
    Base.matches(pattern).mapConstraint(Constraint.Primitive.Matches.apply).mapActual(Data.String.apply)

// def maxItems[F[a] <: Iterable[a], A](reference: Long): SchemaValidation.Collection[F[A], Long, Unit] =
//   Base.maxItems(reference).mapConstraint(Constraint.Collection.MaxItems.apply).mapActual((long, _))

// def minItems[F[a] <: Iterable[a], A](reference: Long): SchemaValidation.Collection[F[A], Long, Unit] =
//   Base.minItems(reference).mapConstraint(Constraint.Collection.MinItems.apply).mapActual((long, _))

// def nonEmpty[F[a] <: Iterable[a] { def tail: F[a] }, A]: SchemaValidation.Collection[F[A], Long, (A, F[A])] =
//   Base.nonEmpty[F, A].mapConstraint(Constraint.Collection.MinItems.apply).mapActual((long, _))

// def uniqueItems[F[a] <: Iterable[a], A](using
//     schema: Schema[?, A]
// ): SchemaValidation.Collection[F[A], NonEmptyList[A], Unit] = Base.uniqueItems
//   .mapConstraint(_ => Constraint.Collection.UniqueItems)
//   .mapActual((schema.collection.apply(nonEmptyList), _))

// def vector[A]: Transformation.Plain[Vector[A], Vector[A]] = Transformation.ask

// def seq[A]: Transformation.Plain[Vector[A], Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)

// def nonEmptySeq[A]: SchemaTransformation.Collection[Vector[A], Long, NonEmptySeq[A]] =
//   seq[A].ivalidate(nonEmpty)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

// def list[A]: Transformation.Plain[Vector[A], List[A]] = vector[A].imap(_.toList)(_.toVector)

// def nonEmptyList[A]: SchemaTransformation.Collection[Vector[A], Long, NonEmptyList[A]] =
//   list[A].ivalidate(nonEmpty)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

// def set[A: Schema[?, *]: Order]: SchemaTransformation.Collection[Vector[A], NonEmptyList[A], Set[A]] =
//   vector[A].ivalidate_(uniqueItems).imap(_.to(Set))(_.toVector)

// def sortedSet[A: Schema[?, *]: Order]: SchemaTransformation.Collection[Vector[A], NonEmptyList[A], SortedSet[A]] =
//   set[A].imap(SortedSet.from)(_.toSet)

// def nonEmptySet[A: Schema.Of: Order]: SchemaTransformation.Collection[
//   Vector[A],
//   NonEmptyList[A] | Long,
//   NonEmptySet[A]
// ] = sortedSet[A]
//   .ivalidate(nonEmpty)({ case (a, as) => as + a })
//   .imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))
