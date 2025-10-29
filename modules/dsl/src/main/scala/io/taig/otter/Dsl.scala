package io.taig.otter

import io.taig.otter.component.CaseInsensitiveComponent
import io.taig.otter.component.IronNumberComponent
import io.taig.otter.component.IronStringComponent
import io.taig.otter.component.JavaTimeComponent
import io.taig.otter.component.JsonComponent
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.syntax.JsonSyntax

trait Dsl extends AllSyntax, JsonSyntax, Keys:
  val json: JsonComponent & CaseInsensitiveComponent[Json.Primitive.String] & JavaTimeComponent[Json.Primitive.String] =
    new JsonComponent
      with CaseInsensitiveComponent[Json.Primitive.String]
      with JavaTimeComponent[Json.Primitive.String] {}

  val iron: IronNumberComponent[Json.Primitive.Number] & IronStringComponent[Json.Primitive.String] =
    new IronNumberComponent[Json.Primitive.Number] with IronStringComponent[Json.Primitive.String] {}

  val jsonSchema: JsonSchemaKeys = JsonSchemaKeys

object Dsl extends Dsl
