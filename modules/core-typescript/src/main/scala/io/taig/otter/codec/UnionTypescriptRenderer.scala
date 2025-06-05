package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Typescript
import io.taig.otter.Union
import cats.Order

final class UnionTypescriptRenderer[S[_], T[_]: Applicative, A: Order](renderer: Renderer[S, T[A]])
    extends Renderer[Union[S, *], T[Typescript[A]]]:
  override def render[B](schema: Union[S, B]): T[Typescript[A]] = schema.schemas
    .traverse(schema => renderer.render(schema.value))
    .map(_.distinct)
    .map(Typescript.Union.apply)
