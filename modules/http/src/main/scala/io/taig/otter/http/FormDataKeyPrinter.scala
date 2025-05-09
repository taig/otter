package io.taig.otter.http

import io.taig.otter.PrimitivePrinter
import io.taig.otter.http.FormData.Key.Primitive

object FormDataKeyPrinter:
  def apply[A](codec: FormData.Key[A], a: A): String = codec match
    case FormData.Key.Primitive(self) => PrimitivePrinter.Unquoted(codec = self, a)
