package io.taig.otter.codec

import cats.Functor
import cats.Semigroupal
import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.Typescript

final class DictionaryTypescriptRenderer[S[_], T[_], U[_]: Functor: Semigroupal](
    key: Renderer[S, U[Typescript]],
    value: Renderer[T, U[Typescript]]
) extends Renderer[Dictionary[S, T, *], U[Typescript]]:
  override def render[A](schema: Dictionary[S, T, A]): U[Typescript] =
    (key.render(schema = schema.key.value), value.render(schema = schema.value.value))
      .mapN(Typescript.Record.apply)
