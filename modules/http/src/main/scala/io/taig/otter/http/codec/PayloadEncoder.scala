package io.taig.otter.http.codec

import io.taig.otter.+
import scala.annotation.nowarn

abstract class PayloadEncoder[-S[_]]:
  def encode[A](schema: S[A], a: A): Array[Byte]

object PayloadEncoder:
  extension [S[_] <: Matchable](self: PayloadEncoder[S])
    @nowarn("msg=anonymous class definition")
    inline def or[T[_] <: Matchable](encoder: PayloadEncoder[T]): PayloadEncoder[S + T] = new PayloadEncoder[S + T]:
      override def encode[A](schema: (S + T)[A], a: A): Array[Byte] = schema match
        case schema: S[A] => self.encode(schema, a)
        case schema: T[A] => encoder.encode(schema, a)
