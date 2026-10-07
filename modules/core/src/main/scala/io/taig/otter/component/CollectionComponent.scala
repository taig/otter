package io.taig.otter.component

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

import scala.collection.immutable.SortedSet

/** Collection constructors shared by JSON and HTTP parameters. CSV cells have no collection alphabet.
  *
  * Non-empty shapes reject empty input with a minimum-one violation. Sets reject duplicates under the read `Order`
  * before construction, accept unsorted input, and write in the schema's write `Order`, regardless of the supplied
  * set's ordering. Validation applies to the constructed read value; encoders do not run validation.
  */
trait CollectionComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  def chain[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, Chain[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S]
  ): F[S, Chain[W], Chain[R]] = F.chained(Reference.later(schema), validation)

  def chain[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S]
  ): F[S, Chain[W], Chain[R]] = chain(schema, Validation.valid)

  def vector[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, Vector[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S]
  ): F[S, Vector[W], Vector[R]] = F.indexed(Reference.later(schema), validation)

  def vector[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S]
  ): F[S, Vector[W], Vector[R]] = vector(schema, Validation.valid)

  def list[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R], validation: Validation[Constraint.Collection, List[R]])(
      using F: CollectionOperation[F[S, *, *], S]
  ): F[S, List[W], List[R]] = F.linked(Reference.later(schema), validation)

  def list[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S]
  ): F[S, List[W], List[R]] = list(schema, Validation.valid)

  def nonEmptyChain[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, NonEmptyChain[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S]
  ): F[S, NonEmptyChain[W], NonEmptyChain[R]] = F.nonEmptyChained(Reference.later(schema), validation)

  def nonEmptyChain[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S]
  ): F[S, NonEmptyChain[W], NonEmptyChain[R]] = nonEmptyChain(schema, Validation.valid)

  def nonEmptyVector[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, NonEmptyVector[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S]
  ): F[S, NonEmptyVector[W], NonEmptyVector[R]] = F.nonEmptyIndexed(Reference.later(schema), validation)

  def nonEmptyVector[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S]
  ): F[S, NonEmptyVector[W], NonEmptyVector[R]] = nonEmptyVector(schema, Validation.valid)

  def nonEmptyList[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, NonEmptyList[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S]
  ): F[S, NonEmptyList[W], NonEmptyList[R]] = F.nonEmptyLinked(Reference.later(schema), validation)

  def nonEmptyList[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S]
  ): F[S, NonEmptyList[W], NonEmptyList[R]] = nonEmptyList(schema, Validation.valid)

  def sortedSet[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, SortedSet[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S],
      W: Order[W],
      R: Order[R]
  ): F[S, SortedSet[W], SortedSet[R]] = F.sorted(Reference.later(schema), W.toOrdering, R.toOrdering, validation)

  def sortedSet[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S],
      Order[W],
      Order[R]
  ): F[S, SortedSet[W], SortedSet[R]] = sortedSet(schema, Validation.valid)

  def nonEmptySet[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Collection, NonEmptySet[R]]
  )(using
      F: CollectionOperation[F[S, *, *], S],
      W: Order[W],
      R: Order[R]
  ): F[S, NonEmptySet[W], NonEmptySet[R]] =
    F.nonEmptySorted(Reference.later(schema), W.toOrdering, R.toOrdering, validation)

  def nonEmptySet[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      CollectionOperation[F[S, *, *], S],
      Order[W],
      Order[R]
  ): F[S, NonEmptySet[W], NonEmptySet[R]] = nonEmptySet(schema, Validation.valid)
