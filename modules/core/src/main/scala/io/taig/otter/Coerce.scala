package io.taig.otter

import cats.arrow.Profunctor

/** Marks a schema as willing to accept a laxer wire representation than it writes. `F` is the type of the schema it
  * wraps; the leniency itself lives in the interpreter.
  */
sealed trait Coerce[+F[- _, + _], -W, +R]:
  def schema: Reference[F, ?, ?]

object Coerce:
  final case class Root[F[- _, + _], W, R](reference: Reference[F, W, R]) extends Coerce[F, W, R]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[- _, + _], W0, R0, W, R](self: Coerce[F, W0, R0], f: R0 => R, g: W => W0)
      extends Coerce[F, W, R]:
    export self.schema

  given [F[- _, + _]] => Profunctor[[w, r] =>> Coerce[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Coerce[F, W0, R0])(f: W => W0)(g: R0 => R): Coerce[F, W, R] =
      Coerce.Modify(self, g, f)
