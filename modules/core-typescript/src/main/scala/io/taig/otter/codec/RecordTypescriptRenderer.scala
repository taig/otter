package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Typescript

final class RecordTypescriptRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[(String, Typescript)]])
    extends Renderer[Record[S, *], T[Typescript]]:
  override def render[A](schema: Record[S, A]): T[Typescript] =
    schema.value.fields.traverse(field => renderer.render(schema = field.value)).map(Typescript.Object.apply)
