package io.taig.otter.codec

import io.taig.otter.http.Parameter
import io.taig.otter.Typescript

object ParameterTypescriptRenderer extends Renderer[Parameter, Typescript]:
  override def render[A](schema: Parameter[A]): Typescript = Typescript.String
