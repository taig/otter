package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.http.Parameter

object ParameterValueAtomParser extends Decoder[Parameter.Schema.Atom, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = ParameterValueAtomPrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = ParameterValueAtomPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Parameter.Schema.Atom[A], value: String): Validated[Violations, A] = schema match
    case Parameter.Schema.Atom.Constant(self)    => constant.decode(schema = self.self, value)
    case Parameter.Schema.Atom.Enumeration(self) => enumeration.decode(schema = self.self, value)
    case Parameter.Schema.Atom.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self.self, value)
    case Parameter.Schema.Atom.Union(self)       => union.decode(schema = self.self, value)
