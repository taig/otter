package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.otter.Effect

object PrimitiveEffectRenderer extends Renderer[Primitive[?, *], Effect[Nothing]]:
  override def render[A](schema: Primitive[?, A]): Effect[Nothing] = schema match
    case _: Primitive.Boolean[?]   => Effect.Boolean
    case _: Primitive.String[?, ?] => Effect.String
    case _: Primitive.Number[?]    => Effect.Number