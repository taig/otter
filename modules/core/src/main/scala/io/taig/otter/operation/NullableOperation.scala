package io.taig.otter.operation
import io.taig.otter.InvariantK
import io.taig.otter.codec.Encoder

trait NullableOperation[Self[_], Value[_]]:
  def nullable[A](value: => Value[A]): Self[Option[A]]

  def nullable[A](value: => Value[A], default: => A): Self[A]

  def encode[A, T](self: Self[A])(encoder: Encoder[Value, T]): Option[T]

object NullableOperation:
  inline def apply[Self[_], Value[_]](using
      operation: NullableOperation[Self, Value]
  ): NullableOperation[Self, Value] = operation

  given [Value[_]]: InvariantK[[f[_]] =>> NullableOperation[f, Value]] with
    extension [G[_]](operation: NullableOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): NullableOperation[H, Value] =
        new NullableOperation[H, Value]:
          override def nullable[A](value: => Value[A]): H[Option[A]] = fK(operation.nullable(value))

          override def nullable[A](value: => Value[A], default: => A): H[A] = fK(operation.nullable(value, default))

          override def encode[A, T](self: H[A])(encoder: Encoder[Value, T]): Option[T] =
            operation.encode(gK(self))(encoder)
