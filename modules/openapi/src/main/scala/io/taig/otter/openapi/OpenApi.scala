package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Context
import io.taig.otter.Dsl

object OpenApi extends Dsl:
  self =>
  final class Metadata extends Context.Metadata:
    sealed abstract class Schema(attributes: Schema.Attributes):
      type Self <: Schema

      abstract class Field[A]:
        def value: A
        protected def update(f: A => A): Self

      object Field:
        def apply[A](a: A)(f: A => Self): Field[A] = new Field[A]:
          override def value: A = a
          override def update(g: A => A): Self = f(g(a))

      final def description: Field[Option[String]] = Field(attributes.description): description =>
        copy(attributes.copy(description = description))

      final def name: Field[Option[String]] = Field(attributes.name): name =>
        copy(attributes.copy(name = name))

      def copy(attributes: Schema.Attributes): Self

    object Schema:
      final case class Attributes(description: Option[String], name: Option[String])

      val Default: Schema.Attributes = Attributes(description = None, name = None)

    final case class Primitive(attributes: Primitive.Attributes) extends Schema(attributes.schema):
      override type Self = Primitive
      def format: Field[Option[String]] = Field(attributes.format): format =>
        copy(attributes.copy(format = format))

      override def copy(schema: Schema.Attributes): Primitive = copy(attributes = attributes.copy(schema = schema))

    object Primitive:
      final case class Attributes(schema: Schema.Attributes, format: Option[String])

      val Default: Primitive.Attributes = Attributes(schema = Schema.Default, format = None)

    final case class Product(attributes: Schema.Attributes) extends Schema(attributes):
      final override type Self = Product

    override val primitive: Primitive = Primitive(Primitive.Default)
    override val product: Product = Product(Schema.Default)
    override def toProduct(schema: Schema): Product = product

  override val metadata: Metadata = new Metadata

object Playground {
  import OpenApi.*

  // type Required[A] = Plain.Primitive[Metadata[metadata.primitive.type, metadata.Primitive], A]

  val x: Primitive[String] = string
  val y: Schema[String] = x

  // val z: Schema[String] = y.name.update(identity)

}
