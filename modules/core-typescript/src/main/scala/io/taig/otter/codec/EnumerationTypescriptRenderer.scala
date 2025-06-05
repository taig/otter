package io.taig.otter.codec

import io.taig.otter.Enumeration
import io.taig.otter.Typescript

final class EnumerationTypescriptRenderer[S[_]](printer: Encoder[S, String])
    extends Renderer[Enumeration[S, *], Typescript[Nothing]]:
  override def render[B](schema: Enumeration[S, B]): Typescript[Nothing] = render(schema = schema.value)

  def render[B](schema: Enumeration.Value[S, B]): Typescript[Nothing] = schema match
    case Enumeration.Value.Modify(self, _, _) => render(schema = self)
    case schema @ Enumeration.Value.Root(reference, mapping) =>
      val values = schema.values
        .map(mapping.apply)
        .map(printer.encode(schema = reference.value, _))

      Typescript.Enumeration(values)
