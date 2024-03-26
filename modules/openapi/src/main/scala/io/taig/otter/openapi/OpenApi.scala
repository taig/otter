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
    override val primitive = new Context.Primitive {
      override val default: X.Primitive = X.Primitive(None, None)
      override def toProduct(metadata: X.Primitive): X.Product = product.default
    }

    override type Product = X.Product
    override val product = new Context.Product {
      override def default: X.Product = ???
      override def toProduct(metadata: X.Product): X.Product = ???
      override def zip(left: X.Product, right: X.Product): X.Product = ???
    }
  }
  override val metadata: Asdf = new Asdf

object Playground {
  import OpenApi.*

  // type Required[A] = Plain.Primitive[Metadata[metadata.primitive.type, metadata.Primitive], A]

  type A = Plain.Primitive[X.Primitive, String]
  type B = Plain.Schema[X.Schema, String]

  val a: A = ???
  val b: B = a

  // val x: Primitive.Required[String] = string
  // val y: Primitive[String] = string
  // val z: Schema[String] = string

}
