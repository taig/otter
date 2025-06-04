package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Zod

final class CollectionZodRenderer[S[_], T[_]: Functor](
    renderer: Renderer[S, T[Zod]]
) extends Renderer[Collection[S, *], T[Zod]]:
  override def render[A](schema: Collection[S, A]): T[Zod] =
    renderer.render(schema = schema.value.schema.value).map(Zod.Array.apply)
