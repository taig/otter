package io.taig.otter.codec

import cats.Functor
import cats.Semigroupal
import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.Zod

final class DictionaryZodRenderer[S[_], T[_], U[_]: Functor: Semigroupal](
    key: Renderer[S, U[Zod]],
    value: Renderer[T, U[Zod]]
) extends Renderer[Dictionary[S, T, *], U[Zod]]:
  override def render[A](schema: Dictionary[S, T, A]): U[Zod] =
    (key.render(schema = schema.value.key.value), value.render(schema = schema.value.value.value))
      .mapN(Zod.Record.apply)
