package io.taig.otter.operation

import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait FieldOperation[Self[_], Value[_]]:
  def apply[A](name: String, value: => Value[A]): Self[A]

  def optional[A](self: Self[A]): Self[Option[A]]

  def optional[A](self: Self[A], default: => A): Self[A]

  def name[A](self: Self[A]): String

  def schema[A](self: Self[A]): Reference[Value, ?]

  def isOptional[A](self: Self[A]): Boolean

object FieldOperation:
  inline def apply[Self[_], Value[_]](using
      operation: FieldOperation[Self, Value]
  ): FieldOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> FieldOperation[f, Value]] with
    extension [G[_]](operation: FieldOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): FieldOperation[H, Value] =
        new FieldOperation[H, Value]:
          override def apply[A](name: String, value: => Value[A]): H[A] = fK(operation(name, value))

          override def optional[A](self: H[A]): H[Option[A]] = fK(operation.optional(gK(self)))

          override def optional[A](self: H[A], default: => A): H[A] = fK(operation.optional(gK(self), default))

          override def name[A](self: H[A]): String = operation.name(gK(self))

          override def schema[A](self: H[A]): Reference[Value, ?] = operation.schema(gK(self))

          override def isOptional[A](self: H[A]): Boolean = operation.isOptional(gK(self))
