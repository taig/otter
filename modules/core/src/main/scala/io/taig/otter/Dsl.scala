package io.taig.otter

import io.taig.otter

trait Types[M <: Metadata]:
  self =>

  val metadata: M

  final type Codec[A] = otter.Codec[metadata.Codec, A]

  final type Primitive[A] = otter.Primitive[metadata.Primitive, A]

  object Primitive:
    final type Optional[A] = otter.Primitive.Optional[metadata.Primitive, A]
    final type Required[A] = otter.Primitive.Required[metadata.Primitive, A]

  final type Product[A] = otter.Product.Of[Codec[?], metadata.Product, A]

  object Product:
    final type Of[C <: Codec[?], A] = otter.Product.Of[C, metadata.Product, A]

trait Schemas[M <: Metadata] extends Types[M]:
  final val string: Primitive.Required[String] =
    otter.Primitive.Required.Root(metadata.primitive.empty, Type.String)

trait Syntax[M <: Metadata] extends Types[M]:
  extension [C <: Codec[A], A](self: C)
    def toProduct: Product.Of[self.Self[metadata.Codec, A], A] = self.toProduct(_ => metadata.product.empty)

  extension [C1 <: Codec[?], A](self: Product.Of[C1, A])
    def zip[C2 <: Codec[?], B](product: Product.Of[C2, B]): Product.Of[self.Of | product.Of, (A, B)] =
      self.zipWith(metadata.product.zip)(product)

class OpenApi extends Metadata:

  override type Codec = OpenApi.Codec
  override type Primitive = OpenApi.Primitive
  override def primitive: Metadata.Primitive[OpenApi.Primitive] = new Metadata.Primitive:
    override val empty: OpenApi.Primitive = OpenApi.Primitive(format = None)

  override type Product = OpenApi.Product
  override def product: Metadata.Product[OpenApi.Product] = new Metadata.Product:
    override def empty: OpenApi.Product = OpenApi.Product()
    override def zip(left: OpenApi.Product, right: OpenApi.Product): OpenApi.Product =
      OpenApi.Product()

object OpenApi:
  abstract class Codec
  final case class Primitive(format: Option[String]) extends Codec
  final case class Product() extends Codec

val x: Types[OpenApi] & Schemas[OpenApi] & Syntax[OpenApi] = ???

object Playground {
  import x.*

  val myStr: Primitive.Required[String] = string

  val a: Product.Of[Primitive[?], String] = ???
  val b: Product[Int] = ???
  val c: Product.Of[Nothing, Unit] = otter.Product.Empty(???)

  a.zip(b)
  b.zip(c)
  // myStr.toProduct(_ => x.metadata.product.empty).zip(c)
}
