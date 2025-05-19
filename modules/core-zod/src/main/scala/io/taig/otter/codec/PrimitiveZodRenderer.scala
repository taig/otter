package io.taig.otter.codec

import io.taig.otter.Primitive

object PrimitiveZodRenderer extends Renderer[Primitive, String]:
  override def render[A](schema: Primitive[A]): String = schema match
    case _: Primitive.Boolean[?] => "z.boolean()"
    case _: Primitive.String[?]  => "z.string()"
    case _: Primitive.Number[?]  => "z.number()"
