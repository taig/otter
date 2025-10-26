package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

final class FieldRenderer[-S[_], T](renderer: Renderer[S, T]) extends Renderer[Field[S, *], (String, T)]:
  override def render[A](schema: Field[S, A]): (String, T) = schema match
    case Field.Default(self, _)   => render(schema = self)
    case Field.Modify(self, _, _) => render(schema = self)
    case Field.Optional(self)     => render(schema = self)
    case Field.Root(name, schema) => (name, renderer.render(schema = schema.value))

object FieldRenderer:
  def apply[S[_], T](renderer: Renderer[S, T]): Renderer[Field[S, *], (String, T)] =
    new FieldRenderer(renderer)
