package io.taig.otter.http

import org.http4s.Headers as Http4sHeaders
import io.taig.otter.Encoder

object Http4sHeadersEncoder extends Encoder[Headers, Http4sHeaders]:
  override def apply[A](codec: Headers[A], a: A): Http4sHeaders = codec match
    case Headers.Empty              => Http4sHeaders.empty
    case Headers.Optional(self)     => a.fold(Http4sHeaders.empty)(apply(codec = self, _))
    case Headers.Modify(self, _, g) => apply(codec = self, g(a))
    case Headers.Root(header)     => Http4sHeaderEncoder(codec = header, a).fold(Http4sHeaders.empty)(Http4sHeaders(_))
    case Headers.Zip(left, right) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
