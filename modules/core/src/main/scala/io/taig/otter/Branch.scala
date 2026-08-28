package io.taig.otter

import cats.arrow.Profunctor

/** A named member of a [[Union]]. `F` is the type of the schema the branch holds. */
sealed trait Branch[+F[- _, + _], -W, +R]:
  def name: String

  def schema: Reference[F, ?, ?]

object Branch:
  final case class Root[F[- _, + _], W, R](name: String, reference: Reference[F, W, R]) extends Branch[F, W, R]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[- _, + _], W0, R0, W, R](self: Branch[F, W0, R0], f: R0 => R, g: W => W0)
      extends Branch[F, W, R]:
    export self.{name, schema}

  given [F[- _, + _]] => Profunctor[[w, r] =>> Branch[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Branch[F, W0, R0])(f: W => W0)(g: R0 => R): Branch[F, W, R] =
      Branch.Modify(self, g, f)
