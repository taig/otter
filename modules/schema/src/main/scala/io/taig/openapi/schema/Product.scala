package io.taig.openapi.schema

import cats.Eq
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation}

sealed abstract class Product[A, B] extends Schema[B]:
  self =>

  final override type Codec = OpenApi.Object
  final override type Self[a] = Product[A, a]

  def fields: Chain[Field[A, ?]]

  def nulls: Product.Nulls
  def modifyNulls(f: Product.Nulls => Product.Nulls): Product[A, B]
  final def withNulls(nulls: Product.Nulls): Product[A, B] = modifyNulls(_ => nulls)
  final def showNulls: Product[A, B] = withNulls(Product.Nulls.Show)
  final def hideNulls: Product[A, B] = withNulls(Product.Nulls.Hide)

  final def product[C](right: Product[A, C]): Product[A, (B, C)] = Product.Zip(this, right, none, none, nulls)

  final transparent inline infix def zip[C](product: Product[A, C]): Product[A, ?] = inline (this, product) match
    case (b: Product[A, Void], c: Product[A, C]) => b.product(c).imap[C] { case (_, c) => c }(c => (Void, c))
    case (b: Product[A, B], c: Product[A, Void]) => b.product(c).imap[B] { case (c, _) => c }(c => (c, Void))
    case (a: Product[A, Tuple], b) =>
      a.product(b).imap[Tuple.Append[B, C]] { case (b, c) => b :* c }(bc => (bc.init, bc.last.asInstanceOf[C]))
    case (b, c) => b.product(c)

  final transparent inline def :*[C](field: Field[A, C]): Product[A, ?] = this zip field.toProduct

  // TODO gimap / as

  final override def ivalidate[C](validation: Validation[B, B, B, C])(g: C => B): Product[A, C] =
    Product.Validate(this, validation, g, example.flatMap(validation.run(_).toOption))

  final override def decode(openapi: OpenApi): Validated[Violations, B] = openapi match
    case openapi: OpenApi.Object => decodeWithRemainders(openapi).map(_._2)
    case _                       => typeViolations("Object", openapi).invalid

  def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)]

object Product:
  final private case class Empty[A](description: Option[String], example: Option[Void], nulls: Nulls)
      extends Product[A, Void]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def fields: Chain[Field[A, ?]] = Chain.empty
    override def modifyDescription(f: Option[String] => Option[String]): Product[A, Void] =
      copy(description = f(description))
    override def modifyExample(f: Option[Void] => Option[Void]): Product[A, Void] = copy(example = f(example))
    override def modifyNulls(f: Nulls => Nulls): Product[A, Void] = copy(nulls = f(nulls))

    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Void)] =
      (openapi, Void).valid
    override def encode(a: Void): OpenApi.Object = OpenApi.Object.Empty

  final private case class Root[A, B](
      description: Option[String],
      example: Option[B],
      field: Field[A, B],
      nulls: Nulls
  ) extends Product[A, B]:
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def fields: Chain[Field[A, ?]] = Chain.one(field)
    override def modifyDescription(f: Option[String] => Option[String]): Product[A, B] =
      copy(description = f(description))
    override def modifyExample(f: Option[B] => Option[B]): Product[A, B] = copy(example = f(example))
    override def modifyNulls(f: Nulls => Nulls): Product[A, B] = copy(nulls = f(nulls))
    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
      field.decode(openapi)
    override def encode(b: B): OpenApi.Object = field.encode(b, nulls)

  final private case class Validate[A, B, C](
      product: Product[A, B],
      validation: Validation[B, B, B, C],
      g: C => B,
      example: Option[C]
  ) extends Product[A, C]:
    override def constraints: Chain[Constraint[OpenApi]] =
      product.constraints ++ validation.constraints.map(_.map(product.encode))
    override def description: Option[String] = product.description
    override def fields: Chain[Field[A, ?]] = product.fields
    override def nulls: Nulls = product.nulls
    override def modifyDescription(f: Option[String] => Option[String]): Product[A, C] =
      copy(product = product.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Product[A, C] = copy(example = f(example))
    override def modifyNulls(f: Nulls => Nulls): Product[A, C] = copy(product = product.modifyNulls(f))

    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, C)] =
      product.decodeWithRemainders(openapi).andThen(_.traverse(andThenValidate(validation, product.encode)))

    override def encode(c: C): OpenApi.Object = product.encode(g(c))

  final private case class Zip[A, B, C](
      left: Product[A, B],
      right: Product[A, C],
      description: Option[String],
      example: Option[(B, C)],
      nulls: Nulls
  ) extends Product[A, (B, C)]:
    override def constraints: Chain[Constraint[OpenApi]] = left.constraints ++ right.constraints
    override def fields: Chain[Field[A, ?]] = left.fields ++ right.fields
    override def modifyDescription(f: Option[String] => Option[String]): Product[A, (B, C)] =
      copy(description = f(description))
    override def modifyExample(f: Option[(B, C)] => Option[(B, C)]): Product[A, (B, C)] = copy(example = f(example))
    override def modifyNulls(f: Nulls => Nulls): Product[A, (B, C)] = copy(nulls = f(nulls))

    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, (B, C))] =
      left.decodeWithRemainders(openapi) match
        case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          left.decodeWithRemainders(openapi).fold(violations merge _, _ => violations).invalid

    override def encode(ab: (B, C)): OpenApi.Object = left.encode(ab._1) ++ right.encode(ab._2)

  enum Nulls:
    case Show
    case Hide

  object Nulls:
    val Default: Product.Nulls = Show

    given Eq[Product.Nulls] = Eq.fromUniversalEquals

  def empty[A]: Product[A, Void] = Empty(none, none, Nulls.Default)

  def apply[A, B](field: Field[A, B]): Product[A, B] = Root(none, none, field, Nulls.Default)
