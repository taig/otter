package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Parameter
import io.taig.otter.TypescriptZod
import io.taig.otter.Zod

object ParameterTypescriptZodRenderer extends Renderer[Parameter, TypescriptZod]:
  override def render[A](schema: Parameter[A]): TypescriptZod = TypescriptZod(
    typescript = Typescript.String,
    zod = Zod.Expression("z.string()")
  )
