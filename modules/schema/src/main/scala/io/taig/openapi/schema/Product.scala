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
) extends Value[A]:
  self =>

  final override type Self[a] = Product[a]
  final override type Codec = OpenApi.Object
  final override type Metadata[a] = Product.Metadata[a]

  // object nulls {}

  final infix def zip[B](product: Product[B]): Product[(A, B)] = ???

  final transparent inline def :*[B](field: Field[B]): Product[?] = inline (this, field) match
    case (left: Product[Void], right: Field[B]) =>
      left.zip(right.toProduct).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left: Product[A], right: Field[Void]) =>
      left.zip(right.toProduct).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Product[Tuple], right) =>
      left
        .zip(right.toProduct)
        .imap[Tuple.Append[A, B]] { case (a, b) => a :* b }(ab => (ab.init, ab.last.asInstanceOf[B]))
    case (left, right) => left.zip(right.toProduct)

  override def copy(metadata: Product.Metadata[A]): Product[A] = new Product[A](constraints, fields, metadata):
    export self.{decodeWithRemainders, encode}

  override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Product[B] = ???

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null            => decode(OpenApi.Object.Empty).orElse(metadata.default.toValid(???))
    case openapi: OpenApi.Object => decode(openapi)
    case _                       => ???
  final def decode(openapi: OpenApi.Object): Validated[Violations, A] = decodeWithRemainders(openapi).map(_._2)

  def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)]

  override def encode(a: A): OpenApi.Object

object Product:
  enum Nulls:
    case Show
    case Hide

  object Nulls:
    val Default: Product.Nulls = Show

    given Eq[Product.Nulls] = Eq.fromUniversalEquals

    given Semigroup[Product.Nulls] with
      override def combine(x: Nulls, y: Nulls): Nulls = if x === y then x else Default

  final case class Metadata[A](
      default: Option[A],
      description: Option[String],
      example: Option[A],
      nulls: Product.Nulls
  ) extends Value.Metadata[A]:
    override type Self[a] = Product.Metadata[a]
    override def updated(default: Option[A], description: Option[String], example: Option[A]): Product.Metadata[A] =
      Metadata(default, description, example, null)
    override def map[B](f: A => B): Product.Metadata[B] = copy(default = default.map(f), example = example.map(f))
    override def flatMap[B](f: A => Option[B]): Product.Metadata[B] =
      copy(default = default.flatMap(f), example = example.flatMap(f))

  object Metadata:
    def empty[A]: Product.Metadata[A] = Metadata(None, None, None, Nulls.Default)

  val Empty: Product[Void] = new Product[Void](Chain.empty, Chain.empty, Metadata.empty):
    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Void)] =
      (openapi, Void).valid
    override def encode(a: Void): OpenApi.Object = OpenApi.Object.Empty

  def fromField[A](field: Field[A]): Product[A] = new Product[A](Chain.empty, Chain.one(field), Metadata.empty):
    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
      field.decode(openapi)
    override def encode(a: A): OpenApi.Object = field.encode(a, metadata.nulls)
