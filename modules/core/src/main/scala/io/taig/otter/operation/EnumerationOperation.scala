package io.taig.otter.operation

import io.taig.enumeration.ext.Mapping
import io.taig.otter.FunctorK

trait EnumerationOperation[+Self[_], -Value[_]]:
  def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B]

object EnumerationOperation:
  inline def apply[Self[_], Value[_]](using
      operation: EnumerationOperation[Self, Value]
  ): EnumerationOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[f[_]] =>> EnumerationOperation[f, Value]] with
    extension [G[_]](operation: EnumerationOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): EnumerationOperation[H, Value] =
        new EnumerationOperation[H, Value]:
          override def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): H[B] =
            fK(operation.enumeration(schema, mapping))
