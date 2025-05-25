package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Header

object HeaderDataEncoder extends Encoder[Header, Option[Header.Data]]:
  override def encode[A](header: Header[A], a: A): Option[Header.Data] = header match
    case Header.Root(name, schema) => (name, HeaderValuePrinter.encode(schema = schema.value, a)).some
    case Header.Modify(self, _, g) => encode(header = self, g(a))
    case Header.Optional(self)     => a.flatMap(encode(header = self, _))
