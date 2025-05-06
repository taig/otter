package io.taig.otter.http

import org.http4s.Entity as Http4sBody
import scodec.bits.ByteVector

import java.nio.charset.Charset

final class Http4sBodyEncoder[F[_], S[_]](encoder: PayloadEncoder[S]):
  // TODO what should I actually pass here rather than charset?
  def apply[A](charset: Option[Charset], body: Body[S, A], a: A): Http4sBody[F] = body match
    case Body.Empty                  => Http4sBody.empty
    case Body.Modify(self, _, g)     => apply(charset, body = self, g(a))
    case Body.Or(left, right)        => ???
    case Body.OrElse(left, right)    => a.fold(apply(charset, body = left, _), apply(charset, body = right, _))
    case Body.Root(mediaType, codec) => Http4sBody.strict(ByteVector(encoder(mediaType, codec.value, a)))
