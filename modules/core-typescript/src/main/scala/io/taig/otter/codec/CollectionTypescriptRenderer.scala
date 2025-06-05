package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Typescript

final class CollectionTypescriptRenderer[S[_], T[_]: Functor, A](
    renderer: Renderer[S, T[A]]
) extends Renderer[Collection[S, *], T[Typescript[A]]]:
  override def render[B](schema: Collection[S, B]): T[Typescript[A]] =
    renderer.render(schema = schema.value.schema.value).map(Typescript.Array.apply)
