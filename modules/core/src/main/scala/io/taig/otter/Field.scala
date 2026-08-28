package io.taig.otter

import cats.Eval
import cats.arrow.Profunctor

/** A named member of a [[Record]]. `F` is the type of the schema the field holds. */
sealed trait Field[+F[-_, +_], -W, +R]:
  def name: String

  def isOptional: Boolean

  def schema: Reference[F, ?, ?]

object Field:
  final case class Root[F[-_, +_], W, R](name: String, reference: Reference[F, W, R]) extends Field[F, W, R]:
    override def isOptional: Boolean = false

    override def schema: Reference[F, ?, ?] = reference

  final case class Optional[F[-_, +_], W, R](self: Field[F, W, R]) extends Field[F, Option[W], Option[R]]:
    export self.{name, schema}

    override def isOptional: Boolean = true

  final case class Default[F[-_, +_], W, R](self: Field[F, W, R], value: Eval[R]) extends Field[F, W, R]:
    export self.{name, schema}

    override def isOptional: Boolean = true

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Field[F, W0, R0], f: R0 => R, g: W => W0)
      extends Field[F, W, R]:
    export self.{isOptional, name, schema}

  given [F[-_, +_]] => Profunctor[[w, r] =>> Field[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Field[F, W0, R0])(f: W => W0)(g: R0 => R): Field[F, W, R] =
      Field.Modify(self, g, f)
