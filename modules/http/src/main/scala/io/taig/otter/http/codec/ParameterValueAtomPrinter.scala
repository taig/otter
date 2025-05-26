package io.taig.otter.http.codec

import io.taig.otter.*
import io.taig.otter.codec.*
import io.taig.otter.http.Parameter

object ParameterValueAtomPrinter extends Encoder[Parameter.Schema.Atom, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Parameter.Schema.Atom[A], a: A): String = schema match
    case Parameter.Schema.Atom.Constant(self)    => constant.encode(schema = self.self, a)
    case Parameter.Schema.Atom.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Parameter.Schema.Atom.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Parameter.Schema.Atom.Union(self)       => union.encode(schema = self.self, a)
