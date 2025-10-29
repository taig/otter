package io.taig.otter.operation

import cats.Eq
import io.taig.otter.InvariantK
import io.taig.otter.Reference
import io.taig.otter.codec.Encoder

trait ConstantOperation[Self[_], Value[_]]:
  def constant[A: Eq](schema: => Value[A], value: A): Self[A]

  def encode[A, T](self: Self[A])(encoder: Encoder[Value, T]): T

  def schema[A](self: Self[A]): Reference[Value, ?]

object ConstantOperation:
  inline def apply[Self[_], Value[_]](using
      operation: ConstantOperation[Self, Value]
  ): ConstantOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> ConstantOperation[f, Value]] with
    extension [G[_]](operation: ConstantOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): ConstantOperation[H, Value] =
        new ConstantOperation[H, Value]:
          override def constant[A: Eq](schema: => Value[A], value: A): H[A] = fK(operation.constant(schema, value))

          override def encode[A, T](self: H[A])(encoder: Encoder[Value, T]): T = operation.encode(gK(self))(encoder)

          override def schema[A](self: H[A]): Reference[Value, ?] = operation.schema(gK(self))
