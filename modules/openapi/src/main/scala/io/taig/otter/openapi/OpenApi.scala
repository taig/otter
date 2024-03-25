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

  object Schema:
    val Default: HMap[Schema] = HMap.Empty.put(description, None).put(name, None)

  type Primitive = Schema | format.type

  object Primitive:
    val Default: HMap[Primitive] = Schema.Default.put(format, None)

  type Product = Schema

  object Product:
    val Default: HMap[Product] = Schema.Default

  type Of[S <: Plain.Schema[?]] <: HMap[Schema] = S match
    case Plain.Primitive[?] => HMap[Primitive]
    case Plain.Product[?]   => HMap[Product]

  val default: [S <: Plain.Schema[?]] => S => Of[S] = [S <: Plain.Schema[?]] =>
    (schema: S) =>
      schema match
        case _: Plain.Primitive[?] => Primitive.Default
        case _: Plain.Product[?]   => Product.Default

  val toProduct: [S[a] <: Plain.Schema[a], A] => Of[S[A]] => Of[Plain.Product[A]] =
    [S[a] <: Plain.Schema[a], A] => (_: Of[S[A]]) => Product.Default

object Playground {
  val x = new Types[MyMetadata.Of]
    with Schemas[MyMetadata.Of](MyMetadata.default)
    with Syntax[MyMetadata.Of](MyMetadata.toProduct) {}

  import x.*

  type MyOpenApi[A] = Primitive[A] | Product[A]

  val y: Primitive[String] = ???
  val z: Product[String] = ???

  y.metadata.apply(MyMetadata.name)
  z.metadata.apply(MyMetadata.description)
  z.metadata.apply(MyMetadata.description, "lol")
  z.metadata.clear(MyMetadata.description)

  toProductWith(z)(_ => MyMetadata.Product.Default)
  z.toProduct
}
