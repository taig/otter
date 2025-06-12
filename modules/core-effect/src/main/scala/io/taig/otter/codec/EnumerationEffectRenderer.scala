package io.taig.otter.codec

import io.taig.otter.Enumeration
import io.taig.otter.Effect

final class EnumerationEffectRenderer[S[_], A](printer: Encoder[S, A]) extends Renderer[Enumeration[S, *], Effect[A]]:
  override def render[B](schema: Enumeration[S, B]): Effect[A] = render(schema = schema.value)

  def render[B](schema: Enumeration.Value[S, B]): Effect[A] = schema match
    case Enumeration.Value.Modify(self, _, _) => render(schema = self)
    case schema @ Enumeration.Value.Root(reference, mapping) =>
      val values = schema.values
        .map(mapping.apply)
        .map(printer.encode(schema = reference.value, _))

      Effect.Union(values)
