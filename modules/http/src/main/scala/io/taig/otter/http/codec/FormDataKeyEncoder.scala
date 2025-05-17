package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.FormData
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.ConstantEncoder

object FormDataKeyPrinter extends Encoder[FormData.Key, String]:
  val constant = ConstantEncoder(encoder = this)

  override def encode[A](schema: FormData.Key[A], a: A): String = schema match
    case FormData.Key.Constant(self)  => ConstantEncoder(encoder = this).encode(schema = self, a)
    case FormData.Key.Primitive(self) => PrimitivePrinter.Unquoted.encode(schema = self, a)
