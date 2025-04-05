package io.taig.otter

object PrimitiveZodRenderer extends Renderer[Primitive, String]:
  override def apply[A](codec: Primitive[A]): String = codec match
    case _: Primitive.Boolean[?] => "z.boolean()"
    case _: Primitive.String[?]  => "z.string()"
    case _: Primitive.Number[?]  => "z.number()"
