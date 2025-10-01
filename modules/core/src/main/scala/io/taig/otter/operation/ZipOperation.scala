package io.taig.otter.operation

import io.taig.otter.InvariantK

trait ZipOperation[Self[_]]:
  self =>

  extension [A](self: Self[A]) def zip[B](schema: Self[B]): Self[(A, B)]

  def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): ZipOperation[G] =
    new ZipOperation[G]:
      extension [A](ga: G[A])
        override def zip[B](schema: G[B]): G[(A, B)] =
          fK(self.zip(gK(ga))(gK(schema)))

object ZipOperation:
  given InvariantK[ZipOperation] with
    extension [G[_]](self: ZipOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): ZipOperation[H] =
        self.imapK(fK)(gK)
