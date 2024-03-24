package io.taig.otter.openapi

import io.taig.otter
import io.taig.otter.Schema
import io.taig.otter.Primitive
import io.taig.otter.Product
import io.taig.otter.HMap

object MyMetadata:
  val description: HMap.Key[Option[String]] = HMap.Key("description")
  val name: HMap.Key[Option[String]] = HMap.Key("name")
  val format: HMap.Key[Option[String]] = HMap.Key("format")

  type Schema = description.type | name.type

  object Schema:
    val Empty: HMap[Schema] = HMap.Empty.put(name, None).put(description, None)

  type Primitive = Schema | format.type

  object Primitive:
    val Empty: HMap[Primitive] = Schema.Empty.put(format, None)

  type Product = Schema

  object Product:
    val Empty: HMap[Product] = Schema.Empty

  type Of[S[a] <: otter.Schema[a]] = S[Any] match
    case otter.Product[?]   => HMap[Product]
    case otter.Primitive[?] => HMap[Primitive]

object Playground {
  val empty: [S[+a] <: Schema[a], A] => S[A] => MyMetadata.Of[S] = [S[+a] <: Schema[a], A] =>
    (schema: S[A]) =>
      schema match
        case _: Product[?]   => MyMetadata.Product.Empty
        case _: Primitive[?] => MyMetadata.Primitive.Empty

  val toProduct: HMap[MyMetadata.Schema] => MyMetadata.Of[Product] = _ => MyMetadata.Product.Empty

//   new Types[Metadata.Of] with Schemas[Metadata.Of](empty) with Syntax[Metadata.Schema, Metadata.Of](toProduct) {}
}
