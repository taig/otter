package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Union
import cats.Order
import io.taig.otter.Effect

final class UnionEffectRenderer[S[_], T[_]: Applicative, A: Order](renderer: Renderer[S, T[A]])
    extends Renderer[Union[S, *], T[Effect[A]]]:
  override def render[B](schema: Union[S, B]): T[Effect[A]] = schema.schemas
    .traverse(schema => renderer.render(schema.value))
    .map(_.distinct)
    .map(Effect.Union.apply)
