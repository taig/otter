package io.taig.otter.http.codec

import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Query

object QueryValueAtomPrinter extends Encoder[Query.Value.Atom, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Query.Value.Atom[A], a: A): String = schema match
    case Query.Value.Atom.Constant(self)    => constant.encode(schema = self.self, a)
    case Query.Value.Atom.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Query.Value.Atom.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Query.Value.Atom.Union(self)       => union.encode(schema = self.self, a)
