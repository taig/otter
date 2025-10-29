package io.taig.otter.codec

import io.taig.otter.Json

object JsonCoerceZodRenderer extends Renderer[Json.Coerce, String]:
  override def render[A](json: Json.Coerce[A]): String = ???
  // json.schema.value match
  //   case Json.Primitive.Boolean(_) => "z.coerce.boolean()"
  //   case Json.Primitive.Number(_)  => "z.coerce.number()"
  //   case Json.Primitive.String(_)  => "z.coerce.string()"
