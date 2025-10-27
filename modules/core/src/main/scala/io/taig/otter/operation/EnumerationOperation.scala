package io.taig.otter.operation

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import io.taig.otter.InvariantK
import io.taig.otter.codec.Encoder

trait EnumerationOperation[Self[_], Value[_]]:
  def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B]

  def encode[A, T](self: Self[A])(encoder: Encoder[Value, T]): NonEmptyList[T]

object EnumerationOperation:
  inline def apply[Self[_], Value[_]](using
      operation: EnumerationOperation[Self, Value]
  ): EnumerationOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> EnumerationOperation[f, Value]] with
    extension [G[_]](operation: EnumerationOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): EnumerationOperation[H, Value] =
        new EnumerationOperation[H, Value]:
          override def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): H[B] =
            fK(operation.enumeration(schema, mapping))

          override def encode[A, T](self: H[A])(encoder: Encoder[Value, T]): NonEmptyList[T] =
            operation.encode(gK(self))(encoder)
