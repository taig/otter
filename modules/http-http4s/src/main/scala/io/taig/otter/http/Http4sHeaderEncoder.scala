package io.taig.otter.http

import io.taig.otter.Encoder
import cats.syntax.all.*
import org.http4s.Header.Raw as Http4sHeader
import io.taig.otter.http.HttpKeys.*

object Http4sHeaderEncoder extends Encoder[Header, Option[Http4sHeader]]:
  override def apply[A](codec: Header[A], a: A): Option[Http4sHeader] = codec match
    case Header.Root(name, codec, metadata) =>
      Http4sHeader(name, value = HttpHeaderPrinter(metadata.get(explode).getOrElse(false))(codec = codec.value, a)).some
    case Header.Modify(self, _, g) => apply(codec = self, g(a))
    case Header.Optional(self)     => a.flatMap(apply(codec = self, _))
