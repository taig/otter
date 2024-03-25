package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.hmap.HMap
import io.taig.hmap.Key
import io.taig.otter.Types
import io.taig.otter.Schemas
import io.taig.otter.Syntax

object MyMetadata:
  val description: Key[Option[String]] = Key("description")
  val name: Key[Option[String]] = Key("name")
  val format: Key[Option[String]] = Key("format")

  type Schema = description.type | name.type

  type Primitive = Schema | format.type

  type Product = Schema

  type Of[S <: Plain.Schema[?]] = S match
    case Plain.Primitive[?] => HMap[Primitive]
    case Plain.Product[?]   => HMap[Product]

  val default: [S <: Plain.Schema[?]] => S => Of[S] = [S <: Plain.Schema[?]] =>
    (schema: S) =>
      schema match
        case _: Plain.Primitive[?] => ??? : HMap[Primitive]
        case _: Plain.Product[?]   => ??? : HMap[Product]

object Playground {

  // val toProduct: HMap[MyMetadata.Schema] => HMap[MyMetadata.Of[Product]] = _ => MyMetadata.Product.Empty

  val x = new Types[MyMetadata.Of] with Schemas[MyMetadata.Of](MyMetadata.default) with Syntax[MyMetadata.Of] {}

  import x.*

  val y: Primitive[String] = ???
  val z: Product[String] = ???

  y.metadata.apply(MyMetadata.name)
  z.metadata.apply(MyMetadata.description)
  z.metadata.apply(MyMetadata.description, "lol")
  z.metadata.clear(MyMetadata.description)

  toProductWith(z)
  z
  // y.metadata.apply(MyMetadata.description)
  // y.metadata.apply(MyMetadata.format)

  // import x.*

  // val a: Primitive[String] = ???
  // val b: Product[String] = ???

  // a.metadata.apply(MyMetadata.format)
  // b.metadata.apply(MyMetadata.format)

  // with Schemas[Metadata.Of](empty) with Syntax[Metadata.Schema, Metadata.Of](toProduct) {}
}
