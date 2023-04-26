package io.taig.openapi.schema

import cats.{Eq, Semigroup}
import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Product[A](
    val constraints: Chain[Constraint[OpenApi]],
    val fields: Chain[Field[?]],
    val metadata: Product.Metadata[A]
) extends Schema[A]:
  self =>

  final override type Self[a] = Product[a]
  final override type Metadata[a] = Product.Metadata[a]

  object nulls:
    def value: Product.Null = metadata.nulls

  final infix def zip[B](product: Product[B]): Product[(A, B)] = ???

  final transparent inline def :*[B](field: Field[B]): Product[?] = ???
//  inline (this, field) match
//    case (left: Product[Void], right: Field[B]) =>
//      left.zip(right.toProduct).imap[B] { case (_, b) => b }(b => (Void, b))
//    case (left: Product[A], right: Field[Void]) =>
//      left.zip(right.toProduct).imap[A] { case (a, _) => a }(a => (a, Void))
//    case (left: Product[Tuple], right) =>
//      left
//        .zip(right.toProduct)
//        .imap[Tuple.Append[A, B]] { case (a, b) => a :* b }(ab => (ab.init, ab.last.asInstanceOf[B]))
//    case (left, right) => left.zip(right.toProduct)

  final override def copy(metadata: Product.Metadata[A]): Product.Of[A, Codec] =
    new Product[A](constraints, fields, metadata) { export self.{decode, encode, Codec} }

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Product.Of[B, Codec] = ???

  final override def optional: Product.Of[Option[A], self.Codec | OpenApi.Null.type] =
    new Product[Option[A]](constraints, fields, metadata.map(_.some)):
      override type Codec = self.Codec | OpenApi.Null.type

      override def decode(openapi: OpenApi): Validated[Violations, Option[A]] = openapi match
        case OpenApi.Null => none[A].valid
        case _            => self.decode(openapi).map(_.some)

      override def encode(a: Option[A]): self.Codec | OpenApi.Null.type = a.fold(OpenApi.Null)(self.encode)

object Product:
  type Of[A, B <: OpenApi] = Product[A] { type Codec = B }

  enum Null:
    case Show
    case Hide

  object Null:
    val Default: Product.Null = Show

    given Eq[Product.Null] = Eq.fromUniversalEquals

    given Semigroup[Product.Null] with
      override def combine(x: Null, y: Null): Null = if x === y then x else Default

  final case class Metadata[A](
      description: Option[String],
      example: Option[A],
      nulls: Product.Null
  ) extends Schema.Metadata[A]:
    override type Self[a] = Product.Metadata[a]
    override def updated(description: Option[String], example: Option[A]): Product.Metadata[A] =
      Metadata(description, example, nulls)
    override def map[B](f: A => B): Product.Metadata[B] = copy(example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Product.Metadata[B] = copy(example = example.flatMap(f))

  object Metadata:
    def empty[A]: Product.Metadata[A] = Metadata(None, None, Null.Default)

  val Empty: Product[Void] = new Product[Void](Chain.empty, Chain.empty, Metadata.empty):
    override type Codec = OpenApi.Object
    override def decode(openapi: OpenApi): Validated[Violations, Void] = ??? // (openapi, Void).valid
    override def encode(a: Void): OpenApi.Object = OpenApi.Object.Empty

  def fromField[A](field: Field[A]): Product[A] = new Product[A](Chain.empty, Chain.one(field), Metadata.empty):
    override type Codec = OpenApi.Object
    override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
      case openapi: OpenApi.Object => ??? // field.decode(openapi)
      case _                       => ???
    override def encode(a: A): OpenApi.Object = field.encode(a, metadata.nulls)
