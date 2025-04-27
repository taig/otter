package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations

final class Http4sBodyDecoder[S[_]](decode: [A] => Array[Byte] => Validated[Violations, S[A]]):
  def apply[A](body: Body[S, A], value: Array[Byte]): Validated[Violations, A] = ???
