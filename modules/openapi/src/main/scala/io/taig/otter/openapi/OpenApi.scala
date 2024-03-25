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
    override val schema: Schema.Metadata[Schema] = new Schema.Metadata[Schema]:
      override val default: HMap[Schema] = HMap.Empty.put(name, None).put(description, None)
      override def toProduct(metadata: HMap[Schema]): HMap[Product] = product.default

    override type Primitive = Schema | format.type
    override val primitive = new Primitive.Metadata:
      override val default: HMap[Primitive] = schema.default.put(format, None)
      override def toProduct(metadata: HMap[Primitive]): HMap[Product] = product.default

    override type Product = Schema
    override val product = new Product.Metadata:
      override val default: HMap[Product] = schema.default
      override def toProduct(metadata: HMap[Product]): HMap[Product] = default
      override def zip(left: HMap[Product], right: HMap[Product]): HMap[Product] = default
