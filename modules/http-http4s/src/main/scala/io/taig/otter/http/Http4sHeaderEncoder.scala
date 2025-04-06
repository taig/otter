package io.taig.otter.http

import io.taig.otter.Encoder
import cats.syntax.all.*
import org.http4s.Header.Raw as Http4sHeader

object Http4sHeaderEncoder extends Encoder[Header, Option[Http4sHeader]]:
  override def apply[A](codec: Header[A], a: A): Option[Http4sHeader] = codec match
    case Header.Array(name, codec, _) =>
      Http4sHeader(name, value = HeaderCodecArrayPrinter(codec = codec.value, a)).some
    case Header.Modify(self, _, g) => apply(codec = self, g(a))
    case Header.Object(name, codec, explode, _) =>
      Http4sHeader(name, value = HeaderCodecObjectPrinter(explode)(codec = codec.value, a)).some
    case Header.Optional(self) => a.flatMap(apply(codec = self, _))
    case Header.Value(name, codec, _) =>
      Http4sHeader(name, value = HeaderCodecPrinter(codec = codec.value, a)).some
