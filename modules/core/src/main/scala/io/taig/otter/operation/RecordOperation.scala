package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait RecordOperation[Self[_], Value[_]]
    extends EmptyOperation[Self],
      LiftOperation[Self, Value],
      ZipOperation[Self, Value]:
  def fields[A](self: Self[A]): Chain[Reference[Value, ?]]

object RecordOperation:
  inline def apply[Self[_], Value[_]](using
      operation: RecordOperation[Self, Value]
  ): RecordOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> RecordOperation[f, Value]] with
    extension [G[_]](operation: RecordOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): RecordOperation[H, Value] =
        new RecordOperation[H, Value]:
          override def empty: H[Unit] = fK(operation.empty)

          override def fields[A](self: H[A]): Chain[Reference[Value, ?]] =
            operation.fields(gK(self))

          override def lift[A](value: => Value[A]): H[A] = fK(operation.lift(value))

          override def zip[A, B](left: H[A], right: H[B]): H[(A, B)] = fK(operation.zip(gK(left), gK(right)))
