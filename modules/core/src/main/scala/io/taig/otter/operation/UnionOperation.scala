package io.taig.otter.operation

import io.taig.otter.InvariantK
import cats.data.NonEmptyChain
import io.taig.otter.Reference

trait UnionOperation[Self[_], Value[_]] extends LiftOperation[Self, Value], OrElseOperation[Self]:
  def schemas[A](self: Self[A]): NonEmptyChain[Reference[Value, ?]]

object UnionOperation:
  inline def apply[Self[_], Value[_]](using
      operation: UnionOperation[Self, Value]
  ): UnionOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> UnionOperation[f, Value]] with
    extension [G[_]](operation: UnionOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): UnionOperation[H, Value] =
        new UnionOperation[H, Value]:
          override def orElse[A, B](left: H[A], right: H[B]): H[Either[A, B]] =
            fK(operation.orElse(gK(left), gK(right)))

          override def lift[A](schema: => Value[A]): H[A] = fK(operation.lift(schema))

          override def schemas[A](self: H[A]): NonEmptyChain[Reference[Value, ?]] = operation.schemas(gK(self))
