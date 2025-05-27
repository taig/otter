package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Parameter

object ParameterTypescriptRenderer extends Renderer[Parameter, Typescript]:
  override def render[A](schema: Parameter[A]): Typescript = Typescript.String
