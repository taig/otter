package io.taig.otter.operation

import io.taig.otter.FunctorK
import io.taig.otter.Reference
import io.taig.otter.InvariantK

trait CoerceOperation[Self[_], Value[_]]:
  def coerce[A](schema: => Value[A]): Self[A]

  def schema[A](self: Self[A]): Reference[Value, ?]

object CoerceOperation:
  inline def apply[Self[_], Value[_]](using
      operation: CoerceOperation[Self, Value]
  ): CoerceOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[s[_]] =>> CoerceOperation[s, Value]] with
    extension [G[_]](operation: CoerceOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CoerceOperation[H, Value] =
        new CoerceOperation[H, Value]:
          override def coerce[A](schema: => Value[A]): H[A] = fK(operation.coerce(schema))

          override def schema[A](self: H[A]): Reference[Value, ?] = operation.schema(gK(self))
