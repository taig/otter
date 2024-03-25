package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.hmap.HMap
import io.taig.hmap.Key
import io.taig.otter.Types
import io.taig.otter.Schemas
import io.taig.otter.Syntax

abstract class OpenApi
    extends Types[OpenApi.Metadata.Attributes]
    with Schemas[OpenApi.Metadata.Attributes](OpenApi.Metadata.default)
    with Syntax[OpenApi.Metadata.Attributes](OpenApi.Metadata.toProduct) {
  export OpenApi.Attribute.*
}

object OpenApi:
  object Attribute:
    val description: Key[Option[String]] = Key("description")
    val name: Key[Option[String]] = Key("name")
    val format: Key[Option[String]] = Key("format")

  object Metadata:
    import Attribute.*

    type Schema = description.type | name.type

    object Schema:
      val Default: HMap[Schema] = HMap.Empty.put(description, None).put(name, None)

    type Primitive = Schema | format.type

    object Primitive:
      val Default: HMap[Primitive] = Schema.Default.put(format, None)

    type Product = Schema

    object Product:
      val Default: HMap[Product] = Schema.Default

    type Attributes[S <: Plain.Schema[?]] <: HMap[Schema] = S match
      case Plain.Primitive[?] => HMap[Primitive]
      case Plain.Product[?]   => HMap[Product]

    val default: [S <: Plain.Schema[?]] => S => Attributes[S] = [S <: Plain.Schema[?]] =>
      (schema: S) =>
        schema match
          case _: Plain.Primitive[?] => Primitive.Default
          case _: Plain.Product[?]   => Product.Default

    val toProduct: [S <: Plain.Schema[?]] => Attributes[S] => Attributes[Plain.Product[?]] =
      ???
      // [S <: Plain.Schema[?]] => (_: Attributes[S]) => Product.Default

object Playground {
  val dsl = new OpenApi {}

  import dsl.*

  val x: Primitive.Required[String] = string
  val y: Primitive[String] = x
  // val z: Schema[String] = y

  y.metadata.apply(name)
  // z.metadata.apply(description)
  // z.metadata.apply(description, "lol")
  // z.metadata.clear(description)

  // z.toProduct
}
