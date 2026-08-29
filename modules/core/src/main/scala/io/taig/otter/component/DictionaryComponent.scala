package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

trait DictionaryComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  def map[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Object, SortedMap[String, R]]
  )(using F: DictionaryOperation[[w, r] =>> F[S, w, r], S]): F[S, SortedMap[String, W], SortedMap[String, R]] =
    F.hashed(Reference.later(schema), validation)

  def map[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      DictionaryOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, SortedMap[String, W], SortedMap[String, R]] = map(schema, Validation.valid)

  def list[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Object, List[(String, R)]]
  )(using F: DictionaryOperation[[w, r] =>> F[S, w, r], S]): F[S, List[(String, W)], List[(String, R)]] =
    F.linked(Reference.later(schema), validation)

  def list[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      DictionaryOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, List[(String, W)], List[(String, R)]] = list(schema, Validation.valid)
