package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations

final class Http4sBodyDecoder[S[_]](decoder: BodyDecoder[S]):
  def apply[A](body: Body[S, A], bytes: Array[Byte]): Validated[Violations, A] = body match
    case Body.Empty               => ().valid
    case Body.Modify(self, f, _)  => apply(body = self, bytes).map(f)
    case Body.Or(left, right)     => ???
    case Body.OrElse(left, right) => ???
    case Body.Root(_, codec)      =>
      // TODO does mediaType make sense here?
      decoder(codec = codec.value, bytes)
