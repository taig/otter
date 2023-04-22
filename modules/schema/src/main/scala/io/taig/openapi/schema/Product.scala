package io.taig.openapi.schema

import cats.{Apply, Eq, Semigroup}
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.validation.{Constraint, Validation}

import scala.Tuple.Concat
import scala.deriving.Mirror

sealed abstract class Product[A](
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[A],
    val description: Option[String],
    val example: Option[A],
    val fields: Chain[Field[?]],
    val name: Option[String],
    val nulls: Product.Nulls
) extends Value[A]:
  self =>
  final override type Self[a] = Product[a]
  final override type Codec = OpenApi.Object

  final def modifyNulls(f: Product.Nulls => Product.Nulls): Product[A] =
    copy(default, description, example, name, f(nulls))
  final def setNulls(nulls: Product.Nulls): Product[A] = self.modifyNulls(_ => nulls)
  final def showNulls: Product[A] = setNulls(Product.Nulls.Show)
  final def hideNulls: Product[A] = setNulls(Product.Nulls.Hide)

  final def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String],
      nulls: Product.Nulls
  ): Product[A] = new Product[A](constraints, default, description, example, fields, name, nulls):
    export self.{decodeWithRemainders, encode}

  final override def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Product[A] = copy(default, description, example, name, nulls)

  override def imap[B](f: A => B)(g: B => A): Product[B] =
    new Product[B](constraints, default.map(f), description, example.map(f), fields, name, nulls):
      override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
        self.decodeWithRemainders(openapi).map(_.map(f))
      override def encode(b: B, nulls: Product.Nulls): OpenApi.Object = self.encode(g(b), nulls)

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Product[C] =
    new Product[C](
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      default.flatMap(validation.run(_).toOption),
      description,
      example.flatMap(validation.run(_).toOption),
      fields,
      name,
      nulls
    ):
      override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, C)] =
        self.decodeWithRemainders(openapi).andThen(_.traverse(andThenValidate(validation, self.encode)))
      override def encode(b: C, nulls: Product.Nulls): OpenApi.Object = self.encode(g(b), nulls)

  final def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Product[B] = imap(evidence.from)(evidence.to)

  final infix def zip[B](product: Product[B]): Product[(A, B)] = new Product[(A, B)](
    self.constraints ++ product.constraints,
    default = none,
    description = none,
    example = none,
    fields = self.fields ++ product.fields,
    name = none,
    nulls = self.nulls |+| product.nulls
  ):
    override def decodeWithRemainders(
        openapi: OpenApi.Object
    ): Validated[Violations, (OpenApi.Object, (A, B))] = self.decodeWithRemainders(openapi) match
      case Validated.Valid((openapi, a)) =>
        product.decodeWithRemainders(openapi).map { case (openapi, b) => (openapi, (a, b)) }
      case Validated.Invalid(violations) =>
        product.decodeWithRemainders(openapi).fold(violations |+| _, _ => violations).invalid
    override def encode(ab: (A, B), nulls: Product.Nulls): OpenApi.Object =
      self.encode(ab._1).deepMerge(product.encode(ab._2))

  final def <*(product: Product[Unit]): Product[A] = zip(product).imap { case (a, _) => a }(a => (a, ()))
  final def <*(field: Field[Unit]): Product[A] = <*(field.toProduct)

  final def *:[B <: Tuple](field: Field[B]): Product[A *: B] =
    zip(field.toProduct).imap { case (a, b) => a *: b } { case a *: b => (a, b) }

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null            => decode(OpenApi.Object.Empty).orElse(default.toValid(nonNullViolations("Object")))
    case openapi: OpenApi.Object => decode(openapi)
    case _                       => typeViolations("Object", openapi).invalid
  final def decode(openapi: OpenApi.Object): Validated[Violations, A] = decodeWithRemainders(openapi).map(_._2)
  def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)]

  final override def encode(a: A): OpenApi.Object = encode(a, nulls)
  protected def encode(a: A, nulls: Product.Nulls): OpenApi.Object

object Product:
  enum Nulls:
    case Show
    case Hide

  object Nulls:
    val Default: Product.Nulls = Show

    given Eq[Product.Nulls] = Eq.fromUniversalEquals

    given Semigroup[Product.Nulls] with
      override def combine(x: Nulls, y: Nulls): Nulls = if x === y then x else Default

  val Empty: Product[Unit] =
    new Product[Unit](Chain.empty, none, none, none, Chain.empty, none, Nulls.Default):
      override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Unit)] =
        (openapi, ()).valid

      override def encode(a: Unit, nulls: Nulls): OpenApi.Object = OpenApi.Object.Empty

  def one[A](field: Field[A]): Product[A] =
    new Product[A](Chain.empty, none, none, none, Chain.one(field), none, Nulls.Default):
      override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, A)] =
        field.decode(openapi)
      override def encode(a: A, nulls: Product.Nulls): OpenApi.Object = field.encode(a, nulls)

  extension [A <: Tuple](self: Product[A])
    def :*[B](field: Field[B]): Product[Tuple.Append[A, B]] =
      self.zip(field.toProduct).imap { case (a, b) => a :* b } { ab =>
        val net = ab.asInstanceOf[NonEmptyTuple]
        (net.init.asInstanceOf[A], net.last.asInstanceOf[B])
      }

  extension (self: Product[Unit])
    def *>[A](product: Product[A]): Product[A] = self.zip(product).imap { case (_, a) => a }(a => ((), a))
    def *>[A](field: Field[A]): Product[A] = *>(field.toProduct)

  given InvariantValidation.Product[Product] with
    override def unit: Product[Unit] = Empty
    override def imap[A, B](fa: Product[A])(f: A => B)(g: B => A): Product[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Product[B])(validation: Validation[A, B, B, C])(
        g: C => B
    ): Product[C] = fa.ivalidate(validation)(g)

    override def product[A, B](fa: Product[A], fb: Product[B]): Product[(A, B)] = fa.zip(fb)
