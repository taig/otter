package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.+
import io.taig.otter.http.header.MediaType

abstract class PayloadDecoder[-S[_]]:
  def apply[A](contentType: MediaType, codec: S[A], bytes: Array[Byte]): Validated[Violations, A]

object PayloadDecoder:
  extension [S[_] <: Matchable](self: PayloadDecoder[S])
    inline def or[T[_] <: Matchable](decoder: PayloadDecoder[T]): PayloadDecoder[S + T] = new PayloadDecoder[S + T]:
      override def apply[A](contentType: MediaType, codec: (S + T)[A], bytes: Array[Byte]): Validated[Violations, A] =
        codec match
          case codec: S[A] => self(contentType, codec, bytes)
          case codec: T[A] => decoder(contentType, codec, bytes)
