package io.taig.otter.openapi

import io.taig.otter
import io.taig.otter.Context
import io.taig.otter.Types
import io.taig.otter.Schema
import io.taig.otter.Primitive
import io.taig.otter.Product
import io.taig.otter.Schemas
import io.taig.otter.Schema
import scala.compiletime.erasedValue
import scala.reflect.classTag
import scala.reflect.ClassTag

// object OpenApi extends Context {
//   override type Codec = Metadata.Schema
//   override type Primitive = Metadata.Primitive
//   override type Product = Metadata.Product

//   override def codec: Context.Codec[Schema, Product] = ???

//   override def primitive: Context.Primitive[Primitive] = ???

//   override def product: Context.Product[Product] = ???
// }

object Metadata {
  abstract class Schema {
    def name: Option[String]
  }
  final case class Primitive(format: Option[String])

  object Primitive:
    val Empty: Metadata.Primitive = Primitive(format = None)

  final case class Product()

  object Product:
    val Empty: Metadata.Product = Product()
}

object Playground {
  type Metadata[S[a] <: Schema[a]] = S[Any] match
    case Product[?]   => Metadata.Product
    case Primitive[?] => Metadata.Primitive

  val empty: [S[a] <: Schema[a]] => S[Any] => Metadata[S] = [S[a] <: Schema[a]] =>
    (schema: S[Any]) =>
      schema match
        case _: Product[?]   => Metadata.Product.Empty
        case _: Primitive[?] => Metadata.Primitive.Empty

  new Types[Metadata] with Schemas[Metadata](empty) {}
}
