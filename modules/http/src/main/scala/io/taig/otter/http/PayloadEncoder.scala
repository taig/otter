package io.taig.otter.http

import io.taig.otter.+

abstract class PayloadEncoder[-S[_]]:
  def apply[A](codec: S[A], a: A): Array[Byte]

object PayloadEncoder:
  extension [S[_] <: Matchable](self: PayloadEncoder[S])
    inline def or[T[_] <: Matchable](encoder: PayloadEncoder[T]): PayloadEncoder[S + T] = new PayloadEncoder[S + T]:
      override def apply[A](codec: (S + T)[A], a: A): Array[Byte] = codec match
        case codec: S[A] => self(codec, a)
        case codec: T[A] => encoder(codec, a)
