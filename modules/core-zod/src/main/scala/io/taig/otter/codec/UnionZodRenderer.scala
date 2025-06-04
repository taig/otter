package io.taig.otter.codec

import cats.data.Chain
import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Zod
import io.taig.otter.Union

final class UnionZodRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[Zod]])
    extends Renderer[Union[S, *], T[Zod]]:
  override def render[A](schema: Union[S, A]): T[Zod] = schema.schemas
    .traverse(schema => renderer.render(schema.value))
    .map(_.distinct)
    .map: values =>
      values.tail match
        case Chain.nil => values.head
        case tail      => Zod.Union(values)
