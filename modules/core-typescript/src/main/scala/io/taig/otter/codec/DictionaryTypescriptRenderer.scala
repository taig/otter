package io.taig.otter.codec

import cats.Functor
import cats.Semigroupal
import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.Typescript

final class DictionaryTypescriptRenderer[S[_], T[_], U[_]: Functor: Semigroupal, A](
    key: Renderer[S, U[A]],
    value: Renderer[T, U[A]]
) extends Renderer[Dictionary[S, T, *], U[Typescript[A]]]:
  override def render[B](schema: Dictionary[S, T, B]): U[Typescript[A]] =
    (key.render(schema = schema.value.key.value), value.render(schema = schema.value.value.value))
      .mapN(Typescript.Record.apply)
