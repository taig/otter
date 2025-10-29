package io.taig.otter.operation

import io.taig.otter.Reference
import io.taig.otter.InvariantK

trait BranchOperation[Self[_], Value[_]]:
  def apply[A](name: String, value: => Value[A]): Self[A]

  def name[A](self: Self[A]): String

  def schema[A](self: Self[A]): Reference[Value, ?]

object BranchOperation:
  inline def apply[Self[_], Value[_]](using operation: BranchOperation[Self, Value]): BranchOperation[Self, Value] =
    operation

  given [Value[_]]: InvariantK[[f[_]] =>> BranchOperation[f, Value]] with
    extension [G[_]](operation: BranchOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): BranchOperation[H, Value] =
        new BranchOperation[H, Value]:
          override def apply[A](name: String, value: => Value[A]): H[A] = fK(operation.apply(name, value))

          override def name[A](self: H[A]): String = operation.name(gK(self))

          override def schema[A](self: H[A]): Reference[Value, ?] = operation.schema(gK(self))
