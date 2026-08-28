package io.taig.otter

import cats.arrow.Profunctor
import io.taig.enumeration.ext.Mapping

/** A closed set of values, each written as one value of the schema `F`. */
sealed trait Enumeration[+F[-_, +_], -W, +R]:
  def schema: Reference[F, ?, ?]

object Enumeration:
  final case class Root[F[-_, +_], A, B](reference: Reference[F, A, A], mapping: Mapping[B, A])
      extends Enumeration[F, B, B]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Enumeration[F, W0, R0], f: R0 => R, g: W => W0)
      extends Enumeration[F, W, R]:
    export self.schema

  given [F[-_, +_]] => Profunctor[[w, r] =>> Enumeration[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Enumeration[F, W0, R0])(f: W => W0)(g: R0 => R): Enumeration[F, W, R] =
      Enumeration.Modify(self, g, f)
