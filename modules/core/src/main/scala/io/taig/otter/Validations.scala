// package io.taig.otter

// import java.util.regex.Pattern
// import cats.data.NonEmptyList
// import cats.data.NonEmptySet
// import cats.data.NonEmptySeq
// import scala.collection.immutable.SortedSet
// import cats.Order
// import cats.implicits.*
// import io.taig.otter.validation.Validations as Base

// trait Validations extends Schemas, Syntax:
//   val email: SchemaValidation.Primitive[String, Nothing, String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

//   def matches(pattern: Pattern): SchemaValidation.Primitive[String, Nothing, String, Unit] =
//     Base.matches(pattern).mapConstraint(Constraint.Primitive.Matches.apply).mapActual(string.toValidationWriter)

//   def maxItems[F[a] <: Iterable[a], A](reference: Long): SchemaValidation.Collection[F[A], Long, Unit] =
//     Base.maxItems(reference).mapConstraint(Constraint.Collection.MaxItems.apply).mapActual(long.toValidationWriter)

//   def minItems[F[a] <: Iterable[a], A](reference: Long): SchemaValidation.Collection[F[A], Long, Unit] =
//     Base.minItems(reference).mapConstraint(Constraint.Collection.MinItems.apply).mapActual(long.toValidationWriter)

//   def nonEmpty[F[a] <: Iterable[a] { def tail: F[a] }, A]: SchemaValidation.Collection[F[A], 0L, (A, F[A])] =
//     Base.nonEmpty[F, A].mapConstraint(Constraint.Collection.MinItems.apply).mapActual(long.toValidationWriter(_))

//   def uniqueItems[F[a] <: Iterable[a], A](using
//       writer: Schema.Writer[A]
//   ): SchemaValidation.Collection[F[A], NonEmptyList[A], Unit] = Base.uniqueItems
//     .mapConstraint(_ => Constraint.Collection.UniqueItems)
//     .mapActual(writer.collection(nonEmptyList).toValidationWriter)

//   def vector[A]: Transformation.Plain[Vector[A], Vector[A]] = Transformation.ask

//   def seq[A]: Transformation.Plain[Vector[A], Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)

//   def nonEmptySeq[A]: SchemaTransformation.Collection[Vector[A], 0L, NonEmptySeq[A]] =
//     seq[A].ivalidate(nonEmpty)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

//   def list[A]: Transformation.Plain[Vector[A], List[A]] = vector[A].imap(_.toList)(_.toVector)

//   def nonEmptyList[A]: SchemaTransformation.Collection[Vector[A], 0L, NonEmptyList[A]] =
//     list[A].ivalidate(nonEmpty)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

//   def set[A: Schema.Writer: Order]: SchemaTransformation.Collection[Vector[A], NonEmptyList[A], Set[A]] =
//     vector[A].validate_(uniqueItems).imap(_.to(Set))(_.toVector)

//   def sortedSet[A: Schema.Writer: Order]: SchemaTransformation.Collection[Vector[A], NonEmptyList[A], SortedSet[A]] =
//     set[A].imap(SortedSet.from)(_.toSet)

//   def nonEmptySet[A: Schema.Writer: Order]: SchemaTransformation.Collection[
//     Vector[A],
//     NonEmptyList[A] | Long,
//     NonEmptySet[A]
//   ] = sortedSet[A]
//     .ivalidate(nonEmpty)({ case (a, as) => as + a })
//     .imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))
