package io.taig.otter.http

import io.taig.otter.http.HttpKeys.*
import cats.syntax.all.*

object HeaderDataEncoder:
  def apply[A](header: Header[A], a: A): Option[Header.Data] = header match
    case Header.Root(name, codec, metadata) =>
      (
        name,
        HttpHeaderPrinter(explode = metadata.get(explode).getOrElse(false))(codec = codec.value, a)
      ).some
    case Header.Modify(self, _, g) => apply(header = self, g(a))
    case Header.Optional(self)     => a.flatMap(apply(header = self, _))
