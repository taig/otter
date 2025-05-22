package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Nullable
import io.taig.otter.Typescript
import cats.Functor
import cats.Applicative

final class NullableTypescriptRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[Typescript]])
    extends Renderer[Nullable[S, *], T[Typescript]]:
  override def render[A](schema: Nullable[S, A]): T[Typescript] =
    schema.schema.fold(Typescript.Void.pure[T]): schema =>
      renderer.render(schema = schema.value).map(Typescript.Nullable.apply)
