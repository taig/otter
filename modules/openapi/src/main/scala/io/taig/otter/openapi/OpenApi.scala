package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Context
import io.taig.otter.Dsl

object OpenApi extends Dsl:
  self =>

  abstract class Field[S, A]:
    def value: A
    protected def update(f: A => A): S

  object Field:
    def apply[S, A](a: A)(f: A => S): Field[S, A] = new Field[S, A]:
      override def value: A = a
      override def update(g: A => A): S = f(g(a))

  final class Metadata extends Context.Metadata:
    sealed abstract class Schema(attributes: Schema.Attributes):
      type Self <: Schema

      type Field[A] = self.Field[Self, A]

      final def description: Field[Option[String]] = Field(attributes.description): description =>
        copy(attributes.copy(description = description))

      final def name: Field[Option[String]] = Field(attributes.name): name =>
        copy(attributes.copy(name = name))

      def copy(attributes: Schema.Attributes): Self

    object Schema:
      final case class Attributes(description: Option[String], name: Option[String])

      val Default: Schema.Attributes = Attributes(description = None, name = None)

    final case class Primitive(attributes: Primitive.Attributes) extends Schema(attributes.schema):
      self =>
      override type Self = Primitive
      def format: self.Field[Option[String]] = Field(attributes.format): format =>
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

  // extension [S <: Plain.Schema[?], M <: metadata.Schema, A](self: Field[M, A]) def modify(f: A => A): S = ???

object Playground {
  import OpenApi.*

  val x: Primitive[String] = string
  val y: Schema[String] = x

  // x.metadata.name

  // val z: Schema[String] = x.name.update(_.map(_.reverse))

}
