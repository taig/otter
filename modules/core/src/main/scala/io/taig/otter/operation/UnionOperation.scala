package io.taig.otter.operation

import cats.data.NonEmptyChain
import io.taig.otter.InvariantK
import io.taig.otter.Reference
import io.taig.otter.Branch

trait UnionOperation[Self[_], Value[_]] extends OrElseOperation[Self]:
  def apply[A](name: String, schema: => Value[A]): Self[A]

  def branches[A](self: Self[A]): NonEmptyChain[Branch[Value, ?]]

object UnionOperation:
  inline def apply[Self[_], Value[_]](using
      operation: UnionOperation[Self, Value]
  ): UnionOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> UnionOperation[f, Value]] with
    extension [G[_]](operation: UnionOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): UnionOperation[H, Value] =
        new UnionOperation[H, Value]:
          override def apply[A](name: String, schema: => Value[A]): H[A] = fK(operation(name, schema))

          override def orElse[A, B](left: H[A], right: H[B]): H[Either[A, B]] =
            fK(operation.orElse(gK(left), gK(right)))

          override def branches[A](self: H[A]): NonEmptyChain[Branch[Value, ?]] = operation.branches(gK(self))
