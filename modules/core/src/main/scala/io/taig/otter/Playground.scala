package io.taig.otter

object Metadata:
  abstract class Schema:
    def name: Option[String]
    def update(f: Option[String] => Option[String]): Any

  final case class Primitive(format: Option[String], name: Option[String]) extends Schema {
    override def update(f: Option[String] => Option[String]): Any = ???
  }

object Playground:
  val c = new Context {

    override type Primitive = Metadata.Primitive
    override val primitive = new Context.Primitive[Primitive] {}

    override type Product = Metadata.Schema

    override val product = new Context.Product {
      override def empty(update: Metadata.Schema => Metadata.Schema): Metadata.Schema = ???
      override def zip(left: Metadata.Schema, right: Metadata.Schema): Metadata.Schema = ???
    }

    override type Codec = Metadata.Schema

    override val codec = new Context.Codec {
      override def toProduct(a: Metadata.Schema): Metadata.Schema = product.empty(???)

    }
  }

  val x: Schemas[c.type] = ???

  val z: Product[String, (String, String)] =
    x.string.toProductWith(_ => ??? : String).zipWith(???)(x.string.toProductWith(_ => ??? : String))
