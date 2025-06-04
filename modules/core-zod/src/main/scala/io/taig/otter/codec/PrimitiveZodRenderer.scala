package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.otter.Zod

object PrimitiveZodRenderer extends Renderer[Primitive[?, *], Zod]:
  override def render[A](schema: Primitive[?, A]): Zod = schema match
    case _: Primitive.Boolean[?]   => Zod.Expression("z.boolean()")
    case _: Primitive.String[?, ?] => Zod.Expression("z.string()")
    case _: Primitive.Number[?]    => Zod.Expression("z.number()")
