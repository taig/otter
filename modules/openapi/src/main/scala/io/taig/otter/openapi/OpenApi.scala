package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.hmap.HMap
import io.taig.hmap.Key
import io.taig.otter.Schemas
import io.taig.otter.Syntax
import io.taig.otter.Context

abstract class OpenApi extends Schemas[OpenApi.Context.type] with Syntax[OpenApi.Context.type]:
  override val context: OpenApi.Context.type = OpenApi.Context

  export OpenApi.Attribute.*

object OpenApi:
  object Attribute:
    val description: Key[Option[String]] = Key("description")
    val name: Key[Option[String]] = Key("name")
    val format: Key[Option[String]] = Key("format")

  object Context extends Plain.Context:
    import Attribute.*

    override type Schema = description.type | name.type
    override val schema = new Plain.Context.Metadata[Schema]:
      override val default: HMap[Schema] = HMap.Empty.put(name, None).put(description, None)

    override type Primitive = Schema | format.type
    override val primitive = new Plain.Context.Metadata[Primitive]:
      override val default: HMap[Primitive] = schema.default.put(format, None)

    override type Product = Schema
    override val product = new Plain.Context.Metadata[Product]:
      override val default: HMap[Product] = schema.default

object Playground {
  val dsl = new OpenApi {}

  import dsl.*

  val x: Primitive.Required[String] = string
  val y: Primitive[String] = x
  val z: Schema[String] = y

  z(name)
  z(name, Some("lol"))
  z.toProduct
}
