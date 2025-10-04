package io.taig.otter.operation

import io.taig.enumeration.ext.Mapping
import io.taig.otter.FunctorK

trait EnumerationOperation[+Self[_], -Value[_]]:
  self =>

  def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B]

  def mapK[G[_]](fK: [A] => Self[A] => G[A]): EnumerationOperation[G, Value] =
    new EnumerationOperation[G, Value]:
      override def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): G[B] =
        fK(self.enumeration(schema, mapping))

object EnumerationOperation:
  given [Value[_]]: FunctorK[[s[_]] =>> EnumerationOperation[s, Value]] with
    extension [G[_]](self: EnumerationOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): EnumerationOperation[H, Value] =
        new EnumerationOperation[H, Value]:
          override def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): H[B] =
            fK(self.enumeration(schema, mapping))
