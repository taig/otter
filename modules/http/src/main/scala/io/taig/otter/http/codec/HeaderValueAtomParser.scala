package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.http.Header

object HeaderValueAtomParser extends Decoder[Header.Value.Atom, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = HeaderValueAtomPrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = HeaderValueAtomPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Header.Value.Atom[A], value: String): Validated[Violations, A] = schema match
    case Header.Value.Atom.Constant(self)    => constant.decode(schema = self.self, value)
    case Header.Value.Atom.Enumeration(self) => enumeration.decode(schema = self.self, value)
    case Header.Value.Atom.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self.self, value)
    case Header.Value.Atom.Union(self)       => union.decode(schema = self.self, value)
