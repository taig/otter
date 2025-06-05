package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Field

final class FieldTypescriptRenderer[S[_], T[_], U[_]: Functor, A](
    key: Encoder[S, String],
    value: Renderer[T, U[A]]
) extends Renderer[Field[S, T, *], U[(String, A)]]:
  override def render[B](schema: Field[S, T, B]): U[(String, A)] =
    value
      .render(schema = schema.value.value.value)
      .tupleLeft(ReferenceConstantRenderer(encoder = key).render(schema.key))
