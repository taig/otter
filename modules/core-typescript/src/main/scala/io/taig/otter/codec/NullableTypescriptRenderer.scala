package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Nullable
import io.taig.otter.Typescript

final class NullableTypescriptRenderer[S[_], T[_]: Applicative, A](renderer: Renderer[S, T[A]])
    extends Renderer[Nullable[S, *], T[Typescript[A]]]:
  override def render[B](schema: Nullable[S, B]): T[Typescript[A]] =
    schema.value.schema.fold(Typescript.Void.pure[T]): schema =>
      renderer.render(schema = schema.value).map(Typescript.Nullable.apply)
