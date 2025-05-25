package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Codec
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.EnumerationDecoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.Query

object QueryValueAtomParser extends Decoder[Query.Value.Atom, String]:
  val constant = ConstantDecoder(codec = Codec(decoder = this, encoder = QueryValueAtomPrinter), render = identity)
  val enumeration =
    EnumerationDecoder(codec = Codec(decoder = this, encoder = QueryValueAtomPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Query.Value.Atom[A], value: String): Validated[Violations, A] = schema match
    case Query.Value.Atom.Constant(self)    => constant.decode(schema = self.self, value)
    case Query.Value.Atom.Enumeration(self) => enumeration.decode(schema = self.self, value)
    case Query.Value.Atom.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self.self, value)
    case Query.Value.Atom.Union(self)       => union.decode(schema = self.self, value)
