package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Nullable
import io.taig.otter.Zod

final class NullableZodRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[Zod]])
    extends Renderer[Nullable[S, *], T[Zod]]:
  override def render[A](schema: Nullable[S, A]): T[Zod] =
    schema.value.schema.fold(Zod.Expression("z.void()").pure[T]): schema =>
      renderer.render(schema = schema.value).map(Zod.Nullable.apply)
