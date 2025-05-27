package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Typescript
import io.taig.otter.Union

final class UnionTypescriptRenderer[S[_], T[_]: Applicative](renderer: Renderer[S, T[Typescript]])
    extends Renderer[Union[S, *], T[Typescript]]:
  override def render[A](schema: Union[S, A]): T[Typescript] = schema.schemas
    .traverse(schema => renderer.render(schema.value))
    .map(_.distinct)
    .map(_.uncons.map(_.uncons))
    .map:
      case (left, Some(right, tail)) =>
        tail.foldLeft(Typescript.Union(left, right))(Typescript.Union.apply)
      case (left, None) => left
