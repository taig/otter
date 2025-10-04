package io.taig.otter.operation

import io.taig.otter.InvariantK

trait TupleOperation[Self[_], -Value[_]] extends EmptyOperation[Self], LiftOperation[Self, Value], ZipOperation[Self]:
  self =>

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): TupleOperation[G, Value] =
    new TupleOperation[G, Value]:
      override def empty: G[Unit] = fK(self.empty)

      override def lift[A](value: => Value[A]): G[A] = fK(self.lift(value))

      extension [A](ga: G[A]) override def zip[B](schema: G[B]): G[(A, B)] = fK(self.zip(gK(ga))(gK(schema)))

object TupleOperation:
  inline def apply[Self[_], Value[_]](using operation: TupleOperation[Self, Value]): TupleOperation[Self, Value] =
    operation

  given [Value[_]]: InvariantK[[s[_]] =>> TupleOperation[s, Value]] with
    extension [G[_]](self: TupleOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleOperation[H, Value] =
        self.imapK(fK)(gK)
