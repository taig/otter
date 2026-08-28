package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

trait DictionaryComponent[F[-_, +_], G[-_, +_]](using F: DictionaryOperation[F, G]):
  def map[W, R](
      schema: => G[W, R],
      validation: Validation[Constraint.Object, SortedMap[String, R]]
  ): F[SortedMap[String, W], SortedMap[String, R]] = F.hashed(Reference.later(schema), validation)

  def map[W, R](schema: => G[W, R]): F[SortedMap[String, W], SortedMap[String, R]] = map(schema, Validation.valid)

  def list[W, R](
      schema: => G[W, R],
      validation: Validation[Constraint.Object, List[(String, R)]]
  ): F[List[(String, W)], List[(String, R)]] = F.linked(Reference.later(schema), validation)

  def list[W, R](schema: => G[W, R]): F[List[(String, W)], List[(String, R)]] = list(schema, Validation.valid)
