package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Validation

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

trait DictionaryComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
    F: DictionaryOperation[F, G]
):
  @targetName("mapValidatedRoundTrip")
  def map[A](
      schema: => G1[A],
      validation: Validation[Constraint.Object, SortedMap[String, A]]
  ): F1[SortedMap[String, A]] = map[A, A](schema, validation)

  def map[W, R](
      schema: => G[W, R],
      validation: Validation[Constraint.Object, SortedMap[String, R]]
  ): F[SortedMap[String, W], SortedMap[String, R]] = F.hashed(Reference.later(schema), validation)

  @targetName("mapRoundTrip")
  def map[A](schema: => G1[A]): F1[SortedMap[String, A]] = map[A, A](schema, Validation.valid)

  def map[W, R](schema: => G[W, R]): F[SortedMap[String, W], SortedMap[String, R]] = map(schema, Validation.valid)

  @targetName("listValidatedRoundTrip")
  def list[A](
      schema: => G1[A],
      validation: Validation[Constraint.Object, List[(String, A)]]
  ): F1[List[(String, A)]] = list[A, A](schema, validation)

  def list[W, R](
      schema: => G[W, R],
      validation: Validation[Constraint.Object, List[(String, R)]]
  ): F[List[(String, W)], List[(String, R)]] = F.linked(Reference.later(schema), validation)

  @targetName("listRoundTrip")
  def list[A](schema: => G1[A]): F1[List[(String, A)]] = list[A, A](schema, Validation.valid)

  def list[W, R](schema: => G[W, R]): F[List[(String, W)], List[(String, R)]] = list(schema, Validation.valid)
