package io.taig.otter

import cats.Eval
import cats.arrow.Profunctor

/** Marks a schema as permitted to be absent. `F` is the type of the schema it wraps. */
sealed trait Optional[+F[- _, + _], -W, +R]:
  def schema: Reference[F, ?, ?]

object Optional:
  final case class Root[F[- _, + _], W, R](reference: Reference[F, W, R]) extends Optional[F, Option[W], Option[R]]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Default[F[- _, + _], W, R](reference: Reference[F, W, R], default: Eval[R])
      extends Optional[F, W, R]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[- _, + _], W0, R0, W, R](self: Optional[F, W0, R0], f: R0 => R, g: W => W0)
      extends Optional[F, W, R]:
    export self.schema

  given [F[- _, + _]] => Profunctor[[w, r] =>> Optional[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Optional[F, W0, R0])(f: W => W0)(g: R0 => R): Optional[F, W, R] =
      Optional.Modify(self, g, f)
