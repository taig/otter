package io.taig.otter.http

import org.http4s.Entity as Http4sBody
import scodec.bits.ByteVector

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

final class Http4sBodyEncoder[F[_], S](encode: S => String):
  // TODO what should I actually pass here rather than charset?
  def apply[A](charset: Option[Charset], body: Body[S, A], a: A): Http4sBody[F] = body match
    case Body.Modify(self, _, g)  => apply(charset, body = self, g(a))
    case Body.Or(left, right)     => ???
    case Body.OrElse(left, right) => a.fold(apply(charset, body = left, _), apply(charset, body = right, _))
    case Body.Root(mediaType, codec) =>
      val bytes = ByteVector(encode(codec).getBytes(charset.getOrElse(StandardCharsets.UTF_8)))
      Http4sBody.strict(bytes)
