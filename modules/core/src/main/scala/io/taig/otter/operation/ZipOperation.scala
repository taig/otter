package io.taig.otter.operation

import io.taig.otter.InvariantK

trait ZipOperation[Self[_]]:
  self =>

  extension [A](self: Self[A]) def zip[B](schema: Self[B]): Self[(A, B)]

  def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): ZipOperation[T] =
    new ZipOperation[T]:
      extension [A](ga: T[A])
        override def zip[B](schema: T[B]): T[(A, B)] =
          fK(self.zip(gK(ga))(gK(schema)))

object ZipOperation:
  given InvariantK[ZipOperation] with
    extension [T[_]](self: ZipOperation[T])
      override def imapK[H[_]](fK: [A] => T[A] => H[A])(gK: [A] => H[A] => T[A]): ZipOperation[H] =
        self.imapK(fK)(gK)
