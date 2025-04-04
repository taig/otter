package io.taig.otter.http

import io.taig.otter.Encoder
import org.http4s.Header.Raw as Http4sHeader
import org.http4s.Headers

object Http4sHeaderEncoder extends Encoder[Header, Option[Http4sHeader]]:
  override def apply[A](codec: Header[A], a: A): Option[Http4sHeader] = codec match
    case Header.Optional(self) => a.flatMap(apply(codec = self, _))
    // case Header.Root(name, codec, metadata) => ???
    // case Header.Collection(self, delimiter) => ???
    case Header.Modify(self, _, g) => apply(codec = self, g(a))
  