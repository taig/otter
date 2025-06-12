package io.taig.otter.codec

import io.taig.otter.Enumeration
import io.taig.otter.Typescript
import cats.syntax.all.*

final class EnumerationTypescriptRenderer[S[_]](printer: Encoder[S, String])
    extends Renderer[Enumeration[S, *], Typescript[Typescript.Value]]:
  override def render[B](schema: Enumeration[S, B]): Typescript[Typescript.Value] = render(schema = schema.value)

  def render[B](schema: Enumeration.Value[S, B]): Typescript[Typescript.Value] = schema match
    case Enumeration.Value.Modify(self, _, _) => render(schema = self)
    case schema @ Enumeration.Value.Root(reference, mapping) =>
      val values = schema.values
        .map(mapping.apply)
        .map(printer.encode(schema = reference.value, _))
        .map(Typescript.Literal.apply)
        .map(Typescript.Value.apply)

      Typescript.Union(values)
