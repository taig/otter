package io.taig.openapi.http

import cats.effect.IO
import fs2.Stream

abstract class StreamWrapper[A] {
  type Effect[_]

  def toStream: Stream[Effect, A]
}
