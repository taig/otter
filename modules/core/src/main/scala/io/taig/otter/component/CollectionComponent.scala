package io.taig.otter.component

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

import scala.annotation.targetName

trait CollectionComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
    F: CollectionOperation[F, G]
):
  @targetName("chainValidatedRoundTrip")
  def chain[A](schema: => G1[A], validation: Validation[Constraint.Collection, Chain[A]]): F1[Chain[A]] =
    chain[A, A](schema, validation)

  def chain[W, R](schema: => G[W, R], validation: Validation[Constraint.Collection, Chain[R]]): F[Chain[W], Chain[R]] =
    F.chained(Reference.later(schema), validation)

  @targetName("chainRoundTrip")
  def chain[A](schema: => G1[A]): F1[Chain[A]] = chain[A, A](schema, Validation.valid)

  def chain[W, R](schema: => G[W, R]): F[Chain[W], Chain[R]] = chain(schema, Validation.valid)

  @targetName("vectorValidatedRoundTrip")
  def vector[A](schema: => G1[A], validation: Validation[Constraint.Collection, Vector[A]]): F1[Vector[A]] =
    vector[A, A](schema, validation)

  def vector[W, R](
      schema: => G[W, R],
      validation: Validation[Constraint.Collection, Vector[R]]
  ): F[Vector[W], Vector[R]] = F.indexed(Reference.later(schema), validation)

  @targetName("vectorRoundTrip")
  def vector[A](schema: => G1[A]): F1[Vector[A]] = vector[A, A](schema, Validation.valid)

  def vector[W, R](schema: => G[W, R]): F[Vector[W], Vector[R]] = vector(schema, Validation.valid)

  @targetName("listValidatedRoundTrip")
  def list[A](schema: => G1[A], validation: Validation[Constraint.Collection, List[A]]): F1[List[A]] =
    list[A, A](schema, validation)

  def list[W, R](schema: => G[W, R], validation: Validation[Constraint.Collection, List[R]]): F[List[W], List[R]] =
    F.linked(Reference.later(schema), validation)

  @targetName("listRoundTrip")
  def list[A](schema: => G1[A]): F1[List[A]] = list[A, A](schema, Validation.valid)

  def list[W, R](schema: => G[W, R]): F[List[W], List[R]] = list(schema, Validation.valid)
