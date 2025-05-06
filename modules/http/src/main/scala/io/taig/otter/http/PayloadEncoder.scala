package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.http.header.MediaType

abstract class PayloadEncoder[-S[_]]:
  def apply[A](contentType: MediaType, codec: S[A], a: A): Array[Byte]

object PayloadEncoder:
  extension [S[_] <: Matchable](self: PayloadEncoder[S])
    inline def or[T[_] <: Matchable](encoder: PayloadEncoder[T]): PayloadEncoder[S + T] = new PayloadEncoder[S + T]:
      override def apply[A](contentType: MediaType, codec: (S + T)[A], a: A): Array[Byte] =
        codec match
          case codec: S[A] => self(contentType, codec, a)
          case codec: T[A] => encoder(contentType, codec, a)
