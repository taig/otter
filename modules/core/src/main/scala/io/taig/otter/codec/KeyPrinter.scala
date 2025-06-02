package io.taig.otter.codec

import io.taig.otter.Key

final class KeyPrinter /*(printer: Encoder[Primitive.Value, String])*/ extends Encoder[Key, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Key[A], a: A): String = ???
  // schema match
  //   case Key.Constant(self)    => constant.encode(schema = self, a)
  //   case Key.Enumeration(self) => enumeration.encode(schema = self, a)
  //   case Key.Primitive(self)   => printer.encode(schema = self.value, a)
  //   case Key.Union(self)       => union.encode(schema = self, a)

object KeyPrinter:
  val Quoted: Encoder[Key, String] = ??? // KeyPrinter(printer = PrimitivePrinter.Quoted)
  val Unquoted: Encoder[Key, String] = ??? // KeyPrinter(printer = PrimitivePrinter.Unquoted)
