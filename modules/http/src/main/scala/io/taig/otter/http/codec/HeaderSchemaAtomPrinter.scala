package io.taig.otter.http.codec

import io.taig.otter.codec.*
import io.taig.otter.http.Header

object HeaderSchemaAtomPrinter extends Encoder[Header.Schema.Atom, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Header.Schema.Atom[A], a: A): String = schema match
    case Header.Schema.Atom.Constant(self)    => constant.encode(schema = self.self, a)
    case Header.Schema.Atom.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Header.Schema.Atom.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Header.Schema.Atom.Union(self)       => union.encode(schema = self.self, a)
