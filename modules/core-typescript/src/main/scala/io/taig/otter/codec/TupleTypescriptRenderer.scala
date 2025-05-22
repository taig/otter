package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Tuple
import io.taig.otter.Typescript
import cats.Applicative

final class TupleTypescriptRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[Typescript]])
    extends Renderer[Tuple[S, *], T[Typescript]]:
  override def render[A](schema: Tuple[S, A]): T[Typescript] = schema.schemas
      .traverse(schema => renderer.render(schema = schema.value))
      .map(Typescript.Tuple.apply)
