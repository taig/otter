package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Typescript

final class CollectionTypescriptRenderer[S[_], T[_]: Functor](
    renderer: Renderer[S, T[Typescript]]
) extends Renderer[Collection[S, *], T[Typescript]]:
  override def render[A](schema: Collection[S, A]): T[Typescript] =
    renderer.render(schema = schema.value.schema.value).map(Typescript.Array.apply)
