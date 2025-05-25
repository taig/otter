package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.http.Parameter

object ParameterValueAtomParser extends Decoder[Parameter.Value.Atom, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = ParameterValueAtomPrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = ParameterValueAtomPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Parameter.Value.Atom[A], value: String): Validated[Violations, A] = schema match
    case Parameter.Value.Atom.Constant(self)    => constant.decode(schema = self.self, value)
    case Parameter.Value.Atom.Enumeration(self) => enumeration.decode(schema = self.self, value)
    case Parameter.Value.Atom.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self.self, value)
    case Parameter.Value.Atom.Union(self)       => union.decode(schema = self.self, value)
