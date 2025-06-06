package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.Key
import io.taig.otter.TypescriptZod

object JsonKeyTypescriptZodRenderer extends Renderer[Key, TypescriptZod]:
  val renderer = KeyTypescriptRenderer(renderer = this).map(TypescriptZod.Shared.apply)

  override def render[A](schema: Key[A]): TypescriptZod = renderer.render(schema)
