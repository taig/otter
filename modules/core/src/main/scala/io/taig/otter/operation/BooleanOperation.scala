package io.taig.otter.operation

import io.taig.otter.FunctorK

trait BooleanOperation[+F[_]]:
  self =>

  def boolean: F[Boolean]

  def mapK[G[_]](fK: [A] => F[A] => G[A]): BooleanOperation[G] = new BooleanOperation[G]:
    override def boolean: G[Boolean] = fK(self.boolean)

object BooleanOperation:
  trait Read[+F[_]] extends BooleanOperation[F]:
    self =>

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): BooleanOperation.Read[G] = new Read[G]:
      override def boolean: G[Boolean] = fK(self.boolean)

  object Read:
    inline def apply[F[_]](using self: BooleanOperation.Read[F]): BooleanOperation.Read[F] = self

    given FunctorK[BooleanOperation.Read] with
      extension [G[_]](self: BooleanOperation.Read[G])
        override def mapK[H[_]](fK: [A] => G[A] => H[A]): BooleanOperation.Read[H] = self.mapK(fK)

  trait Write[+F[_]] extends BooleanOperation[F]:
    self =>

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): BooleanOperation.Write[G] = new Write[G]:
      override def boolean: G[Boolean] = fK(self.boolean)

  object Write:
    inline def apply[F[_]](using self: BooleanOperation.Write[F]): BooleanOperation.Write[F] = self

    given FunctorK[BooleanOperation.Write] with
      extension [G[_]](self: BooleanOperation.Write[G])
        override def mapK[H[_]](fK: [A] => G[A] => H[A]): BooleanOperation.Write[H] = self.mapK(fK)

  inline def apply[F[_]](using self: BooleanOperation[F]): BooleanOperation[F] = self

  given FunctorK[BooleanOperation] with
    extension [G[_]](self: BooleanOperation[G])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): BooleanOperation[H] = self.mapK(fK)
