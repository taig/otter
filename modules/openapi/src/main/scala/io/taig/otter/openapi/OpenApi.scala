package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Context
import io.taig.otter.Dsl

object OpenApi extends Dsl:
  object X:
    sealed abstract class Schema:
      type Self <: X.Schema

      abstract class Field[A]:
        def value: A
        def update(f: A => A): Self

      def name: Field[Option[String]]

    sealed abstract class Primitive extends X.Schema:
      final override type Self = X.Primitive
      def format: Field[Option[String]]

    object Primitive:
      def apply(_format: Option[String], _name: Option[String]): X.Primitive = new Primitive {
        override def name: Field[Option[String]] = new Field[Option[String]]:
          override def value: Option[String] = _name
          override def update(f: Option[String] => Option[String]): X.Primitive =
            Primitive(_format, f(_name))

        override def format: Field[Option[String]] = ???
      }

    sealed abstract class Product extends X.Schema:
      final override type Self = X.Product

  final class Asdf extends Context.Metadata {
    override type Schema = X.Schema
    override type Primitive = X.Primitive
    override type Product = X.Product

    override def primitive: Primitive = ???
    override def product: Product = ???
    override def toProduct(schema: Schema): Product = ???
  }
  override val metadata: Asdf = new Asdf

object Playground {
  import OpenApi.*

  // type Required[A] = Plain.Primitive[Metadata[metadata.primitive.type, metadata.Primitive], A]

  val x: Primitive[String] = string
  val y: Schema[String] = x

}
