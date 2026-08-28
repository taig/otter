package io.taig.otter

import cats.Eval
import cats.arrow.Profunctor
import cats.data.Chain

/** An ordered collection of positional schemas. `F` is the type of the schemas it holds. */
sealed trait Tuple[+F[- _, + _], -W, +R]:
  def schemas: Chain[Reference[F, ?, ?]]

object Tuple:
  case object Empty extends Tuple[Nothing, Unit, Unit]:
    override def schemas: Chain[Reference[Nothing, ?, ?]] = Chain.empty

  final case class Root[F[- _, + _], W, R](schema: Reference[F, W, R]) extends Tuple[F, W, R]:
    override def schemas: Chain[Reference[F, ?, ?]] = Chain.one(schema)

  final case class Product[F[- _, + _], W1, R1, W2, R2](left: Tuple[F, W1, R1], right: Tuple[F, W2, R2])
      extends Tuple[F, (W1, W2), (R1, R2)]:
    override def schemas: Chain[Reference[F, ?, ?]] = left.schemas ++ right.schemas

  final case class Optional[F[- _, + _], W, R](self: Tuple[F, W, R]) extends Tuple[F, Option[W], Option[R]]:
    export self.schemas

  final case class Default[F[- _, + _], W, R](self: Tuple[F, W, R], value: Eval[R]) extends Tuple[F, W, R]:
    export self.schemas

  final case class Modify[F[- _, + _], W0, R0, W, R](self: Tuple[F, W0, R0], f: R0 => R, g: W => W0)
      extends Tuple[F, W, R]:
    export self.schemas

  given [F[- _, + _]] => Profunctor[[w, r] =>> Tuple[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Tuple[F, W0, R0])(f: W => W0)(g: R0 => R): Tuple[F, W, R] =
      Tuple.Modify(self, g, f)

  given [F[- _, + _]] => Zip[[w, r] =>> Tuple[F, w, r]]:
    override def zip[W1, R1, W2, R2](left: Tuple[F, W1, R1], right: Tuple[F, W2, R2]): Tuple[F, (W1, W2), (R1, R2)] =
      Tuple.Product(left, right)
