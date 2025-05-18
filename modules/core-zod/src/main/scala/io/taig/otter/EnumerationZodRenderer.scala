package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.codec.Encoder

final class EnumerationZodRenderer[S[_]](printer: Encoder[S, String]) extends Renderer[Enumeration[S, *], String]:
  override def render[T](schema: Enumeration[S, T]): String = schema match
    case Enumeration.Modify(self, _, _) => render(schema = self)
    case schema @ Enumeration.Root(reference, mapping, _) =>
      val values = schema.values.map(mapping.apply).map(a => printer.encode(schema = reference.value, a))
      s"z.enum([${values.mkString_(", ")}])"
