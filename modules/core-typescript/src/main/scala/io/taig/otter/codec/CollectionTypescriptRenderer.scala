package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.Collection
import cats.syntax.all.*
import cats.Functor

final class CollectionTypescriptRenderer[S[_], T[_]: Functor](
    renderer: Renderer[S, T[Typescript]]
) extends Renderer[Collection[S, *], T[Typescript]]:
  override def render[A](schema: Collection[S, A]): T[Typescript] =
    renderer.render(schema = schema.schema.value).map(Typescript.Array.apply)
