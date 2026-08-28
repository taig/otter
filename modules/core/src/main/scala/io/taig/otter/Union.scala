package io.taig.otter

import cats.arrow.Profunctor
import cats.data.NonEmptyChain

/** A choice between named schemas. `F` is the type of the branches it holds. */
sealed trait Union[+F[-_, +_], -W, +R]:
  def branches: NonEmptyChain[Reference[F, ?, ?]]

object Union:
  final case class Root[F[-_, +_], W, R](branch: Reference[F, W, R]) extends Union[F, W, R]:
    override def branches: NonEmptyChain[Reference[F, ?, ?]] = NonEmptyChain.one(branch)

  final case class Coproduct[F[-_, +_], W1, R1, W2, R2](left: Union[F, W1, R1], right: Union[F, W2, R2])
      extends Union[F, Either[W1, W2], Either[R1, R2]]:
    override def branches: NonEmptyChain[Reference[F, ?, ?]] = left.branches ++ right.branches

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Union[F, W0, R0], f: R0 => R, g: W => W0)
      extends Union[F, W, R]:
    export self.branches

  given [F[-_, +_]] => Profunctor[[w, r] =>> Union[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Union[F, W0, R0])(f: W => W0)(g: R0 => R): Union[F, W, R] =
      Union.Modify(self, g, f)

  given [F[-_, +_]] => Alt[[w, r] =>> Union[F, w, r]]:
    override def alt[W1, R1, W2, R2](
        left: Union[F, W1, R1],
        right: Union[F, W2, R2]
    ): Union[F, Either[W1, W2], Either[R1, R2]] = Union.Coproduct(left, right)
