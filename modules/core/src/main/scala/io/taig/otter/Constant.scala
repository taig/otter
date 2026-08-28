package io.taig.otter

import cats.Eq
import cats.Eval
import cats.arrow.Profunctor

/** A schema that carries a fixed value: it writes that value and, when reading, requires it to be present. `F` is the
  * type of the schema describing the value, which must be bidirectional.
  */
sealed trait Constant[+F[- _, + _], -W, +R]:
  def schema: Reference[F, ?, ?]

object Constant:
  final case class Root[F[- _, + _], A](reference: Reference[F, A, A], value: Eval[A], eq: Eq[A])
      extends Constant[F, Unit, Unit]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[- _, + _], W0, R0, W, R](self: Constant[F, W0, R0], f: R0 => R, g: W => W0)
      extends Constant[F, W, R]:
    export self.schema

  given [F[- _, + _]] => Profunctor[[w, r] =>> Constant[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Constant[F, W0, R0])(f: W => W0)(g: R0 => R): Constant[F, W, R] =
      Constant.Modify(self, g, f)
