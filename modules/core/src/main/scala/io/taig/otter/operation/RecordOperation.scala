package io.taig.otter.operation

import io.taig.otter.InvariantK

trait RecordOperation[Self[_], -Field[_]] extends LiftOperation[Self, Field], ZipOperation[Self]:
  self =>

  def empty: Self[Unit]

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): RecordOperation[G, Field] =
    new RecordOperation[G, Field]:
      override def empty: G[Unit] = fK(self.empty)

      override def lift[A](value: => Field[A]): G[A] = fK(self.lift(value))

      extension [A](ga: G[A]) override def zip[B](schema: G[B]): G[(A, B)] = fK(self.zip(gK(ga))(gK(schema)))

object RecordOperation:
  inline def apply[Self[_], Field[_]](using operation: RecordOperation[Self, Field]): RecordOperation[Self, Field] =
    operation

  given [Field[_]]: InvariantK[[s[_]] =>> RecordOperation[s, Field]] with
    extension [G[_]](self: RecordOperation[G, Field])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): RecordOperation[H, Field] =
        self.imapK(fK)(gK)
