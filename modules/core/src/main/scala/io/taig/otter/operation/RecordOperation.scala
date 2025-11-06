package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.InvariantK
import io.taig.otter.Field

trait RecordOperation[Self[_], Value[_]] extends EmptyOperation[Self], ZipOperation[Self, Value]:
  def fields[A](self: Self[A]): Chain[Field[Value, ?]]

  def lift[A](field: Field[Value, A]): Self[A]

object RecordOperation:
  inline def apply[Self[_], Value[_]](using
      operation: RecordOperation[Self, Value]
  ): RecordOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> RecordOperation[f, Value]] with
    extension [G[_]](operation: RecordOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): RecordOperation[H, Value] =
        new RecordOperation[H, Value]:
          override def empty: H[Unit] = fK(operation.empty)

          override def fields[A](self: H[A]): Chain[Field[Value, ?]] = operation.fields(gK(self))

          override def lift[A](field: Field[Value, A]): H[A] = fK(operation.lift(field))

          override def zip[A, B](left: H[A], right: H[B]): H[(A, B)] = fK(operation.zip(gK(left), gK(right)))
