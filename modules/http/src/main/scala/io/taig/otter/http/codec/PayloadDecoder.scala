package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.+
import io.taig.otter.Violations

import java.nio.charset.Charset
import scala.annotation.nowarn

abstract class PayloadDecoder[-S[_]]:
  def decode[A](schema: S[A], charset: Option[Charset], bytes: Array[Byte]): Validated[Violations, A]

object PayloadDecoder:
  extension [S[_] <: Matchable](self: PayloadDecoder[S])
    @nowarn("msg=anonymous class definition")
    inline def or[T[_] <: Matchable](decoder: PayloadDecoder[T]): PayloadDecoder[S + T] = new PayloadDecoder[S + T]:
      override def decode[A](
          schema: (S + T)[A],
          charset: Option[Charset],
          bytes: Array[Byte]
      ): Validated[Violations, A] = schema match
        case schema: S[A] => self.decode(schema, charset, bytes)
        case schema: T[A] => decoder.decode(schema, charset, bytes)
