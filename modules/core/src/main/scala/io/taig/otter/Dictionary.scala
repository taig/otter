package io.taig.otter

import cats.arrow.Profunctor
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

/** A homogeneous mapping from text keys to values. `F` is the type of the value schema. */
sealed trait Dictionary[+F[-_, +_], -W, +R]:
  def schema: Reference[F, ?, ?]

object Dictionary:
  final case class Hashed[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Object, SortedMap[String, R]]
  ) extends Dictionary[F, SortedMap[String, W], SortedMap[String, R]]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Linked[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Object, List[(String, R)]]
  ) extends Dictionary[F, List[(String, W)], List[(String, R)]]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Dictionary[F, W0, R0], f: R0 => R, g: W => W0)
      extends Dictionary[F, W, R]:
    export self.schema

  given [F[-_, +_]] => Profunctor[[w, r] =>> Dictionary[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Dictionary[F, W0, R0])(f: W => W0)(g: R0 => R): Dictionary[F, W, R] =
      Dictionary.Modify(self, g, f)
