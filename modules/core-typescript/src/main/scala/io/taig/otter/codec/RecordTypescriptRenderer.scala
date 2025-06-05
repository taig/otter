package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Typescript

final class RecordTypescriptRenderer[S[_], T[_]: Applicative, A](renderer: Renderer[S, T[(String, A)]])
    extends Renderer[Record[S, *], T[Typescript[A]]]:
  override def render[B](schema: Record[S, B]): T[Typescript[A]] = schema.value.fields
    .traverse(field => renderer.render(schema = field.value))
    .map(Typescript.Object.apply)
