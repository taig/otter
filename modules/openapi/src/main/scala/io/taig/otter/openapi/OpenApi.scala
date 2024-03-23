package io.taig.otter.openapi

import io.taig.otter.Context

object OpenApi extends Context:
  override type Codec = Metadata.Codec
  override val codec: Context.Codec[Metadata.Codec, Product] = new Context.Codec:
    override def toProduct(codec: Metadata.Codec): Metadata.Product = product.empty

  override type Primitive = Metadata.Primitive
  override val primitive: Context.Primitive[Primitive] = new Context.Primitive:
    override val empty: Metadata.Primitive = Metadata.Primitive(
      format = None,
      name = new Metadata.Field[Metadata.Primitive, Option[String]]:
        override def value: Option[String] = None
        override def apply(value: Option[String]): Metadata.Primitive = ???
    )

  override type Product = Metadata.Product
  override val product: Context.Product[Product] = new Context.Product:
    override val empty: Metadata.Product =
      Metadata.Product(name = new Metadata.Field[Metadata.Product, Option[String]]:
        override def value: Option[String] = None
        override def apply(value: Option[String]): Metadata.Product = ???
      )
    override def zip(left: Metadata.Product, right: Metadata.Product): Metadata.Product = empty

object Metadata:
  abstract class Field[A, B]:
    def value: B
    def apply(value: B): A

  sealed abstract class Codec:
    self =>
    type Self <: Codec

    def name: Field[Self, Option[String]]

  final case class Primitive(format: Option[String], name: Field[Metadata.Primitive, Option[String]])
      extends Metadata.Codec:
    override type Self = Metadata.Primitive

  final case class Product(name: Field[Metadata.Product, Option[String]]) extends Metadata.Codec:
    override type Self = Metadata.Product
