package io.taig.otter

import cats.arrow.Profunctor
import cats.data.Chain

/** An unordered collection of named schemas. `F` is the type of the fields it holds. */
sealed trait Record[+F[-_, +_], -W, +R]:
  def fields: Chain[Reference[F, ?, ?]]

object Record:
  case object Empty extends Record[Nothing, Unit, Unit]:
    override def fields: Chain[Reference[Nothing, ?, ?]] = Chain.empty

  final case class Root[F[-_, +_], W, R](field: Reference[F, W, R]) extends Record[F, W, R]:
    override def fields: Chain[Reference[F, ?, ?]] = Chain.one(field)

  final case class Product[F[-_, +_], W1, R1, W2, R2](left: Record[F, W1, R1], right: Record[F, W2, R2])
      extends Record[F, (W1, W2), (R1, R2)]:
    override def fields: Chain[Reference[F, ?, ?]] = left.fields ++ right.fields

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Record[F, W0, R0], f: R0 => R, g: W => W0)
      extends Record[F, W, R]:
    export self.fields

  given [F[-_, +_]] => Profunctor[[w, r] =>> Record[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Record[F, W0, R0])(f: W => W0)(g: R0 => R): Record[F, W, R] =
      Record.Modify(self, g, f)

  given [F[-_, +_]] => Zip[[w, r] =>> Record[F, w, r]]:
    override def zip[W1, R1, W2, R2](
        left: Record[F, W1, R1],
        right: Record[F, W2, R2]
    ): Record[F, (W1, W2), (R1, R2)] = Record.Product(left, right)
