package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Zod

final class RecordZodRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[(String, Zod)]])
    extends Renderer[Record[S, *], T[Zod]]:
  override def render[A](schema: Record[S, A]): T[Zod] =
    schema.value.fields.traverse(field => renderer.render(schema = field.value)).map(Zod.Object.apply)
