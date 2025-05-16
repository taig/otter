package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Union

object JsonKeyPrinter extends Encoder[Json.Key, String]:
  override def apply[A](schema: Json.Key[A], a: A): String = schema match
    case Json.Key.Constant(self)    => ConstantEncoder(encoder = this)(schema = self, a)
    case Json.Key.Enumeration(self) => EnumerationEncoder(encoder = this)(schema = self, a)
    case Json.Key.Primitive(self)   => PrimitivePrinter.Unquoted(schema = self, a)
    case Json.Key.Union(self)       => apply(schema = self, a)

  def apply[A](schema: Union[Json.Key, A], a: A): String = schema match
    case Union.Root(schema, _)        => apply(schema = schema.value, a)
    case Union.OrElse(left, right, _) => a.fold(apply(left, _), apply(right, _))
    case Union.Modify(self, _, g)     => apply(schema = self, g(a))
