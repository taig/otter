package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

/** Constructs the dictionary type `F` over keys of type `K` and value schemas of type `G`. */
trait DictionaryOperation[F[-_, +_], K[-_, +_], G[-_, +_]]:
  def hashed[KW, KR, W, R](
      key: Reference[K, KW, KR],
      schema: Reference[G, W, R],
      ordering: Ordering[KR],
      validation: Validation[Constraint.Object, SortedMap[KR, R]]
  ): F[SortedMap[KW, W], SortedMap[KR, R]]

  def linked[KW, KR, W, R](
      key: Reference[K, KW, KR],
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Object, List[(KR, R)]]
  ): F[List[(KW, W)], List[(KR, R)]]

  extension [W, R](fa: F[W, R])
    def key: Reference[K, ?, ?]
    def schema: Reference[G, ?, ?]

object DictionaryOperation:
  inline def apply[F[-_, +_], K[-_, +_], G[-_, +_]](using
      self: DictionaryOperation[F, K, G]
  ): DictionaryOperation[F, K, G] = self
