package io.taig.otter.http.codec

import io.taig.otter.*
import io.taig.otter.codec.*
import io.taig.otter.http.Parameter

object ParameterValueAtomPrinter extends Encoder[Parameter.Value.Atom, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Parameter.Value.Atom[A], a: A): String = schema match
    case Parameter.Value.Atom.Constant(self)    => constant.encode(schema = self.self, a)
    case Parameter.Value.Atom.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Parameter.Value.Atom.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Parameter.Value.Atom.Union(self)       => union.encode(schema = self.self, a)
