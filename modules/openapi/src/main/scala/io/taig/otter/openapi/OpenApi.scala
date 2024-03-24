package io.taig.otter.openapi

import io.taig.otter
import io.taig.otter.Schema
import io.taig.otter.Primitive
import io.taig.otter.Product
import io.taig.hmap.HMap
import io.taig.hmap.Key
import io.taig.otter.Types

object MyMetadata:
  val description: Key[Option[String]] = Key("description")
  val name: Key[Option[String]] = Key("name")
  val format: Key[Option[String]] = Key("format")

  type Schema = description.type | name.type

  object Schema:
    val Empty: HMap[Schema] = HMap.Empty.put(name, None).put(description, None)

  type Primitive = Schema | format.type

  object Primitive:
    val Empty: HMap[Primitive] = Schema.Empty.put(format, None)

  type Product = Schema

  object Product:
    val Empty: HMap[Product] = Schema.Empty

  type Of[S[+a] <: otter.Schema[a]] = S[Nothing] match
    case otter.Product[?]   => Product
    case otter.Primitive[?] => Primitive

object Playground {
  // val empty: [S <: Schema[?]] => S => HMap[_ >: MyMetadata.Of[S]] = [S <: Schema[?]] =>
  //   (schema: S) =>
  //     schema match
  //       // case _: Product[A] => MyMetadata.Product.Empty
  //       case _: Primitive[?] => MyMetadata.Primitive.Empty

  val toProduct: HMap[MyMetadata.Schema] => HMap[MyMetadata.Of[Product]] = _ => MyMetadata.Product.Empty

  new Types[MyMetadata.Of] {}
  // with Schemas[Metadata.Of](empty) with Syntax[Metadata.Schema, Metadata.Of](toProduct) {}
}
