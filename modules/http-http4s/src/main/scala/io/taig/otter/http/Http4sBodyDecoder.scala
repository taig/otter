package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import org.http4s.Entity as Http4sBody

final class Http4sBodyDecoder[S](decode: Array[Byte] => Validated[Violations, S]):
  def apply[A](body: Body[S, A], value: Array[Byte]): Validated[Violations, A] = ???
