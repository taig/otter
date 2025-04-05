package io.taig.otter

object JsonPrimitiveZodRenderer extends Renderer[Json.Primitive, String]:
  override def apply[T](codec: Json.Primitive[T]): String =
    PrimitiveZodRenderer(codec = codec.value)
