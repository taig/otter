package io.taig.openapi.schema
import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Product[A](val metadata: Product.Metadata[A]) extends Value[A]:
  self =>

  final override type Self[a] = Product[a]
  final override type Codec = OpenApi.Object
  final override type Metadata[a] = Product.Metadata[a]

  transparent inline def :*[B](field: Field[B]): Product[?] = ???

  override def copy(metadata: Product.Metadata[A]): Product[A] = new Product[A](metadata):
    export self.{decode, encode}

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Product[B] = ???

  override final def decode(openapi: OpenApi): Validated[Violations, A] = ???

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)]

  override def encode(a: A): OpenApi.Object

object Product:
  final case class Metadata[A](
      constraints: Chain[Constraint[OpenApi]],
      default: Option[A],
      description: Option[String],
      example: Option[A]
  ) extends Value.Metadata[A]:
    override type Self[a] = Product.Metadata[a]
    override def updated(default: Option[A], description: Option[String], example: Option[A]): Product.Metadata[A] =
      Metadata(constraints, default, description, example)
    override def map[B](f: A => B): Product.Metadata[B] = copy(default = default.map(f), example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Product.Metadata[B] =
      copy(default = default.flatMap(f), example = example.flatMap(f))
    override def append(constraints: Chain[Constraint[OpenApi]]): Product.Metadata[A] =
      copy(constraints = this.constraints ++ constraints)
