package io.taig.otter.component

import cats.Order
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

/** `K` is the type of the key schema, which is a text primitive: a key is text on the wire whatever it carries, so
  * `dictionary.map(int, …)` is a type error and an integer key is spelled `codec("int", …)`.
  */
trait DictionaryComponent[Bound[-_, +_], K[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  def map[S[-w, +r] <: Bound[w, r], KW, KR, W, R](
      key: K[KW, KR],
      schema: => S[W, R],
      validation: Validation[Constraint.Object, SortedMap[KR, R]]
  )(using
      F: DictionaryOperation[[w, r] =>> F[S, w, r], K, S],
      KR: Order[KR]
  ): F[S, SortedMap[KW, W], SortedMap[KR, R]] =
    F.hashed(Reference.now(key), Reference.later(schema), KR.toOrdering, validation)

  def map[S[-w, +r] <: Bound[w, r], KW, KR, W, R](key: K[KW, KR], schema: => S[W, R])(using
      DictionaryOperation[[w, r] =>> F[S, w, r], K, S],
      Order[KR]
  ): F[S, SortedMap[KW, W], SortedMap[KR, R]] = map(key, schema, Validation.valid)

  def map[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Object, SortedMap[String, R]]
  )(using
      F: DictionaryOperation[[w, r] =>> F[S, w, r], K, S],
      K: PrimitiveOperation.Text[K]
  ): F[S, SortedMap[String, W], SortedMap[String, R]] = map(K.string(Validation.valid), schema, validation)

  def map[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      DictionaryOperation[[w, r] =>> F[S, w, r], K, S],
      PrimitiveOperation.Text[K]
  ): F[S, SortedMap[String, W], SortedMap[String, R]] = map(schema, Validation.valid)

  def list[S[-w, +r] <: Bound[w, r], KW, KR, W, R](
      key: K[KW, KR],
      schema: => S[W, R],
      validation: Validation[Constraint.Object, List[(KR, R)]]
  )(using F: DictionaryOperation[[w, r] =>> F[S, w, r], K, S]): F[S, List[(KW, W)], List[(KR, R)]] =
    F.linked(Reference.now(key), Reference.later(schema), validation)

  def list[S[-w, +r] <: Bound[w, r], KW, KR, W, R](key: K[KW, KR], schema: => S[W, R])(using
      DictionaryOperation[[w, r] =>> F[S, w, r], K, S]
  ): F[S, List[(KW, W)], List[(KR, R)]] = list(key, schema, Validation.valid)

  def list[S[-w, +r] <: Bound[w, r], W, R](
      schema: => S[W, R],
      validation: Validation[Constraint.Object, List[(String, R)]]
  )(using
      F: DictionaryOperation[[w, r] =>> F[S, w, r], K, S],
      K: PrimitiveOperation.Text[K]
  ): F[S, List[(String, W)], List[(String, R)]] = list(K.string(Validation.valid), schema, validation)

  def list[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      DictionaryOperation[[w, r] =>> F[S, w, r], K, S],
      PrimitiveOperation.Text[K]
  ): F[S, List[(String, W)], List[(String, R)]] = list(schema, Validation.valid)
