package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Header
import io.taig.otter.codec.*

object HeaderSchemaAtomParser extends Decoder[Header.Schema.Atom, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = HeaderSchemaAtomPrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = HeaderSchemaAtomPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Header.Schema.Atom[A], value: String): Validated[Violations, A] = schema match
    case Header.Schema.Atom.Constant(self)    => constant.decode(schema = self.self, value)
    case Header.Schema.Atom.Enumeration(self) => enumeration.decode(schema = self.self, value)
    case Header.Schema.Atom.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self.self, value)
    case Header.Schema.Atom.Union(self)       => union.decode(schema = self.self, value)
