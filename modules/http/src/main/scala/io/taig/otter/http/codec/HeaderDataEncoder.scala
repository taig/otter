package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Header

object HeaderDataEncoder extends Encoder[Header, Option[Header.Data]]:
  override def encode[A](header: Header[A], a: A): Option[Header.Data] = encode(header = header.value, a)

  def encode[A](header: Header.Value[A], a: A): Option[Header.Data] = header match
    case Header.Value.Root(name, schema) => (name, HeaderSchemaPrinter.encode(schema = schema.value, a)).some
    case Header.Value.Modify(self, _, g) => encode(header = self, g(a))
    case Header.Value.Optional(self)     => a.flatMap(encode(header = self, _))
