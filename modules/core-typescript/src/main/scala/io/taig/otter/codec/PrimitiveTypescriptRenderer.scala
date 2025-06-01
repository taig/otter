package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.otter.Typescript

object PrimitiveTypescriptRenderer extends Renderer[Primitive[?, *], Typescript]:
  override def render[A](schema: Primitive[?, A]): Typescript = schema match
    case _: Primitive.Boolean[?]   => Typescript.Boolean
    case _: Primitive.String[?, ?] => Typescript.String
    case _: Primitive.Number[?]    => Typescript.Number
