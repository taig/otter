package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Tuple
import io.taig.otter.Zod

final class TupleZodRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[Zod]])
    extends Renderer[Tuple[S, *], T[Zod]]:
  override def render[A](schema: Tuple[S, A]): T[Zod] = schema.value.schemas
    .traverse(schema => renderer.render(schema = schema.value))
    .map(Zod.Tuple.apply)
