package io.taig.otter.http.codec

import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Query

object QueryValueAtomPrinter extends Encoder[Query.Schema.Atom, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Query.Schema.Atom[A], a: A): String = schema match
    case Query.Schema.Atom.Constant(self)    => constant.encode(schema = self.self, a)
    case Query.Schema.Atom.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Query.Schema.Atom.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Query.Schema.Atom.Union(self)       => union.encode(schema = self.self, a)
