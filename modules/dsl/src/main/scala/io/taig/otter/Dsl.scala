package io.taig.otter

import io.taig.otter.component.JsonComponent
import io.taig.otter.syntax.JsonSyntax
import io.taig.otter.component.CaseInsensitiveComponent
import io.taig.otter.component.IronNumberComponent
import io.taig.otter.component.IronStringComponent
import io.taig.otter.syntax.AllSyntax

trait Dsl extends AllSyntax, JsonSyntax, Keys:
  val json: JsonComponent & CaseInsensitiveComponent[Json.Primitive.String] & IronNumberComponent[
    Json.Primitive.Number
  ] & IronStringComponent[Json.Primitive.String] =
    new JsonComponent
      with CaseInsensitiveComponent[Json.Primitive.String]
      with IronNumberComponent[Json.Primitive.Number]
      with IronStringComponent[Json.Primitive.String] {}

  val jsonSchema: JsonSchemaKeys = JsonSchemaKeys

object Dsl extends Dsl
