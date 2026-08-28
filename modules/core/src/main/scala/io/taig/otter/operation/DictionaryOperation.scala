package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

/** Constructs the dictionary type `F` over value schemas of type `G`. */
trait DictionaryOperation[F[- _, + _], G[- _, + _]]:
  def hashed[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Object, SortedMap[String, R]]
  ): F[SortedMap[String, W], SortedMap[String, R]]

  def linked[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Object, List[(String, R)]]
  ): F[List[(String, W)], List[(String, R)]]

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object DictionaryOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: DictionaryOperation[F, G]): DictionaryOperation[F, G] = self
