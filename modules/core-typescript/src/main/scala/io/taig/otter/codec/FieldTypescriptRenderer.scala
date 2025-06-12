package io.taig.otter.codec

import io.taig.otter.Field
import cats.syntax.all.*
import cats.data.Chain
import cats.Applicative

final class FieldTypescriptRenderer[S[_], T[_], U[_]: Applicative, A](
    printer: Encoder[S, String],
    renderer: Renderer[T, U[A]]
) extends Renderer[Field[S, T, *], U[(String, A)]]:
  override def render[B](schema: Field[S, T, B]): U[(String, A)] =
    renderer.render(schema.value.value.value).tupleLeft(ReferenceConstantRenderer(printer).render(schema.key))
