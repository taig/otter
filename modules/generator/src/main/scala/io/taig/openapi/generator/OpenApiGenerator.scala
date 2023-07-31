package io.taig.openapi.generator

import io.taig.openapi.OpenApi
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Primitive, Type}

object OpenApiGenerator {
  def primitive(schema: Primitive[?]): OpenApi = OpenApi.obj(
    "type" := tpe(schema.tpe),
    "format" := schema.format.value,
    "description" := schema.description.value,
    "example" := schema.example.encode
  )

  val tpe: Type[?] => String =
    case Type.Boolean => "boolean"
    case Type.Int     => "integer"
    case Type.String  => "string"
}
