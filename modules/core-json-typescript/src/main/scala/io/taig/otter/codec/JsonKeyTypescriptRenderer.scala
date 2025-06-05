package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.Key
import io.taig.otter.Typescript.Value

object JsonKeyTypescriptRenderer extends Renderer[Key, Typescript.Value]:
  val renderer = KeyTypescriptRenderer(renderer = this).map(Typescript.Value.apply)

  override def render[A](schema: Key[A]): Typescript.Value = renderer.render(schema)
