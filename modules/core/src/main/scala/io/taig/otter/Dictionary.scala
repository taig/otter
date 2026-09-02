package io.taig.otter

import cats.arrow.Profunctor
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

/** A homogeneous mapping from keys to values. `K` is the type of the key schema, `F` the type of the value schema.
  *
  * A key is text on the wire whatever it carries, so its schema writes the key and reads it back independently: the
  * encoder only ever uses the write side and the decoder only the read side, and they never meet. That is why the two
  * are separate parameters rather than one, and why only the read side carries the [[Ordering]] a [[SortedMap]] is
  * built with.
  */
sealed trait Dictionary[+K[-_, +_], +F[-_, +_], -W, +R]:
  def key: Reference[K, ?, ?]

  def schema: Reference[F, ?, ?]

object Dictionary:
  final case class Hashed[K[-_, +_], F[-_, +_], KW, KR, W, R](
      keys: Reference[K, KW, KR],
      reference: Reference[F, W, R],
      ordering: Ordering[KR],
      validation: Validation[Constraint.Object, SortedMap[KR, R]]
  ) extends Dictionary[K, F, SortedMap[KW, W], SortedMap[KR, R]]:
    override def key: Reference[K, ?, ?] = keys

    override def schema: Reference[F, ?, ?] = reference

  final case class Linked[K[-_, +_], F[-_, +_], KW, KR, W, R](
      keys: Reference[K, KW, KR],
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Object, List[(KR, R)]]
  ) extends Dictionary[K, F, List[(KW, W)], List[(KR, R)]]:
    override def key: Reference[K, ?, ?] = keys

    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[K[-_, +_], F[-_, +_], W0, R0, W, R](self: Dictionary[K, F, W0, R0], f: R0 => R, g: W => W0)
      extends Dictionary[K, F, W, R]:
    export self.{key, schema}

  given [K[-_, +_], F[-_, +_]] => Profunctor[[w, r] =>> Dictionary[K, F, w, r]]:
    override def dimap[W0, R0, W, R](
        self: Dictionary[K, F, W0, R0]
    )(f: W => W0)(g: R0 => R): Dictionary[K, F, W, R] = Dictionary.Modify(self, g, f)
