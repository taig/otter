package io.taig.otter.operation

import io.taig.otter.InvariantK

trait TupleOperation[Self[_], -Value[_]]
    extends EmptyOperation[Self],
      LiftOperation[Self, Value],
      ZipOperation[Self, Value]

object TupleOperation:
  inline def apply[Self[_], Value[_]](using
      operation: TupleOperation[Self, Value]
  ): TupleOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> TupleOperation[f, Value]] with
    extension [G[_]](operation: TupleOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): TupleOperation[H, Value] =
        new TupleOperation[H, Value]:
          override def empty: H[Unit] = fK(operation.empty)

          override def lift[A](value: => Value[A]): H[A] = fK(operation.lift(value))

          override def zip[A, B](left: H[A], right: H[B]): H[(A, B)] = fK(operation.zip(gK(left), gK(right)))
