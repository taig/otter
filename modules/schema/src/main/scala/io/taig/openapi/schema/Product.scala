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

  final override type Codec = OpenApi.Object
  final override type Self[a] = Product[a]
  final override type Metadata[a] = Product.Metadata[a]

  object nulls extends Attribute[Product.Null](metadata.nulls):
    override def updated(f: Product.Null => Product.Null): Product.Metadata[A] =
      metadata.copy(nulls = f(metadata.nulls))
    def show: Product[A] = set(Product.Null.Show)
    def hide: Product[A] = set(Product.Null.Hide)

  final def product[B](b: Product[B]): Product[(A, B)] = new Product[(A, B)](
    self.constraints ++ b.constraints,
    self.fields ++ b.fields,
    metadata.flatMap(_ => None)
  ):
    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, (A, B))] =
      self.decodeWithRemainders(openapi) match
        case Validated.Valid((remainders, a)) => b.decodeWithRemainders(remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          b.decodeWithRemainders(openapi).fold(violations merge _, _ => violations).invalid

    override def encode(ab: (A, B)): OpenApi.Object = self.encode(ab._1) ++ b.encode(ab._2)

  final transparent inline infix def zip[B](b: Product[B]): Product[?] = inline (this, b) match
    case (a: Product[Void], b: Product[B]) => a.product(b).imap[B] { case (_, b) => b }(b => (Void, b))
    case (a: Product[A], b: Product[Void]) => a.product(b).imap[A] { case (a, _) => a }(a => (a, Void))
    case (a: Product[Tuple], b) =>
      a.product(b).imap[Tuple.Append[A, B]] { case (a, b) => a :* b }(ab => (ab.init, ab.last.asInstanceOf[B]))
    case (a, b) => a.product(b)

  final transparent inline def :*[B](field: Field[B]): Product[?] = this zip field.toProduct

  final override def copy(metadata: Product.Metadata[A]): Product[A] =
    new Product[A](constraints, fields, metadata) { export self.{decodeWithRemainders, encode} }

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Product[B] =
    new Product[B](
      constraints ++ validation.constraints.map(_.map(self.encode)),
      fields,
      metadata.flatMap(validation.run(_).toOption)
    ):
      override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
        self.decodeWithRemainders(openapi).andThen(_.traverse(andThenValidate(validation, self.encode)))
      override def encode(b: B): self.Codec = self.encode(g(b))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case openapi: OpenApi.Object => decodeWithRemainders(openapi).map(_._2)
    case _                       => typeViolations("Object", openapi).invalid

  def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)]

object Product:
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
    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Void)] =
      (openapi, Void).valid
    override def encode(a: Void): OpenApi.Object = OpenApi.Object.Empty

  def fromField[A](field: Field[A]): Product[A] = new Product[A](Chain.empty, Chain.one(field), Metadata.empty):
    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
      field.decode(openapi)
    override def encode(a: A): OpenApi.Object = field.encode(a, metadata.nulls)
