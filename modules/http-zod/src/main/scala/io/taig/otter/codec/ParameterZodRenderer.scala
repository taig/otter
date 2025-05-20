package io.taig.otter.codec

import io.taig.otter.http.Parameter

object ParameterZodRenderer extends Renderer[Parameter, String]:
  override def render[A](schema: Parameter[A]): String = "z.string()"
