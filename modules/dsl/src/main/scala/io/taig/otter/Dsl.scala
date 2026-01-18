package io.taig.otter

import io.taig.otter.component.IronPrimitiveComponent
import io.taig.otter.component.JsonComponent
import io.taig.otter.syntax.AllSyntax
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.*
import io.taig.otter.component.JavaTimeComponent

trait Dsl extends AllSyntax, Keys:
  object iron extends IronPrimitiveComponent

  object json extends JsonComponent, JavaTimeComponent[Json.Primitive.Text]

  // val jsonSchema: JsonSchemaKeys = JsonSchemaKeys

object Dsl extends Dsl

object Playground:
  import Dsl.*

  val _: Json.Primitive.Text[String :| MinLength[5]] = iron.text[MinLength[5]](json.string)
  val _: Json.Primitive.Number.Read[Int :| LessEqual[10]] =
    iron.number[LessEqual[10]](json.int(_): Json.Primitive.Number.Read[Int])
