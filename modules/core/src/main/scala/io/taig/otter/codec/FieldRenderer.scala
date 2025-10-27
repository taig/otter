package io.taig.otter.codec

import io.taig.otter.Field
import cats.Functor
import cats.syntax.all.*

final class FieldRenderer[-S[_], F[_]: Functor, T](renderer: Renderer[S, F[T]])
    extends Renderer[Field[S, *], F[(String, T)]]:
  override def render[A](schema: Field[S, A]): F[(String, T)] = schema match
    case Field.Default(self, _)   => render(schema = self)
    case Field.Modify(self, _, _) => render(schema = self)
    case Field.Optional(self)     => render(schema = self)
    case Field.Root(name, schema) => renderer.render(schema = schema.value).tupleLeft(name)

object FieldRenderer:
  def apply[S[_], F[_]: Functor, T](renderer: Renderer[S, F[T]]): Renderer[Field[S, *], F[(String, T)]] =
    new FieldRenderer(renderer)
