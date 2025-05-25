package io.taig.otter.http.codec

import io.taig.otter.codec.*
import io.taig.otter.http.Header

object HeaderValueAtomPrinter extends Encoder[Header.Value.Atom, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Header.Value.Atom[A], a: A): String = schema match
    case Header.Value.Atom.Constant(self)    => constant.encode(schema = self.self, a)
    case Header.Value.Atom.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Header.Value.Atom.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Header.Value.Atom.Union(self)       => union.encode(schema = self.self, a)
