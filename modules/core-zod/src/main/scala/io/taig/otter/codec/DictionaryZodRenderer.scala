package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState

final class DictionaryZodRenderer[S[_], T[_]](
    key: Renderer[S, ZodState[ZodExpression]],
    value: Renderer[T, ZodState[ZodExpression]]
) extends Renderer[Dictionary[S, T, *], ZodState[String]]:
  override def render[A](schema: Dictionary[S, T, A]): ZodState[String] =
    (key.render(schema = schema.key.value), value.render(schema = schema.value.value))
      .mapN((key, value) => show"z.record($key, $value)")
