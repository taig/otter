//package io.taig.openapi.schema
//
//import cats.{Eq, Semigroup}
//import cats.syntax.all.*
//import cats.data.{Chain, Validated}
//import io.taig.openapi.OpenApi
//import io.taig.validation.{Constraint, Validation}
//
//sealed abstract class Product[A, B](
//    val constraints: Chain[Constraint[OpenApi]],
//    val fields: Chain[Field[A, ?]],
//    val metadata: Product.Metadata[B]
//) extends Schema[B]:
//  self =>
//
//  final override type Codec = OpenApi.Object
//  final override type Self[a] = Product[A, a]
//  final override type Metadata[a] = Product.Metadata[a]
//
//  object nulls extends Attribute[Product.Null](metadata.nulls):
//    override def updated(f: Product.Null => Product.Null): Product.Metadata[B] =
//      metadata.copy(nulls = f(metadata.nulls))
//    def show: Product[A, B] = set(Product.Null.Show)
//    def hide: Product[A, B] = set(Product.Null.Hide)
//
//  final def product[C](c: Product[A, C]): Product[A, (B, C)] = new Product[A, (B, C)](
//    self.constraints ++ c.constraints,
//    self.fields ++ c.fields,
//    metadata.flatMap(_ => None)
//  ):
//    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, (B, C))] =
//      self.decodeWithRemainders(openapi) match
//        case Validated.Valid((remainders, a)) => c.decodeWithRemainders(remainders).map(_.tupleLeft(a))
//        case Validated.Invalid(violations) =>
//          c.decodeWithRemainders(openapi).fold(violations merge _, _ => violations).invalid
//    override def encode(bc: (B, C)): OpenApi.Object = self.encode(bc._1) ++ c.encode(bc._2)
//
//  final transparent inline infix def zip[C](c: Product[A, C]): Product[A, ?] = inline (this, c) match
//    case (b: Product[A, Void], c: Product[A, C]) => b.product(c).imap[C] { case (_, c) => c }(c => (Void, c))
//    case (b: Product[A, B], c: Product[A, Void]) => b.product(c).imap[B] { case (c, _) => c }(c => (c, Void))
//    case (a: Product[A, Tuple], b) =>
//      a.product(b).imap[Tuple.Append[B, C]] { case (b, c) => b :* c }(bc => (bc.init, bc.last.asInstanceOf[C]))
//    case (b, c) => b.product(c)
//
//  final transparent inline def :*[C](field: Field[A, C]): Product[A, ?] = this zip field.toProduct
//
//  final override def copy(metadata: Product.Metadata[B]): Product[A, B] =
//    new Product[A, B](constraints, fields, metadata) { export self.{decodeWithRemainders, encode} }
//
//  final override def ivalidate[C](validation: Validation[B, B, B, C])(g: C => B): Product[A, C] =
//    new Product[A, C](
//      constraints ++ validation.constraints.map(_.map(self.encode)),
//      fields,
//      metadata.flatMap(validation.run(_).toOption)
//    ):
//      override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, C)] =
//        self.decodeWithRemainders(openapi).andThen(_.traverse(andThenValidate(validation, self.encode)))
//      override def encode(b: C): self.Codec = self.encode(g(b))
//
//  final override def decode(openapi: OpenApi): Validated[Violations, B] = openapi match
//    case openapi: OpenApi.Object => decodeWithRemainders(openapi).map(_._2)
//    case _                       => typeViolations("Object", openapi).invalid
//
//  def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)]
//
//object Product:
//  enum Null:
//    case Show
//    case Hide
//
//  object Null:
//    val Default: Product.Null = Show
//
//    given Eq[Product.Null] = Eq.fromUniversalEquals
//
//    given Semigroup[Product.Null] with
//      override def combine(x: Null, y: Null): Null = if x === y then x else Default
//
//  final case class Metadata[A](
//      description: Option[String],
//      example: Option[A],
//      nulls: Product.Null
//  ) extends Schema.Metadata[A]:
//    override type Self[a] = Product.Metadata[a]
//    override def updated(description: Option[String], example: Option[A]): Product.Metadata[A] =
//      Metadata(description, example, nulls)
//    override def map[B](f: A => B): Product.Metadata[B] = copy(example = example.map(f))
//    override def flatMap[B](f: A => Option[B]): Product.Metadata[B] = copy(example = example.flatMap(f))
//
//  object Metadata:
//    def empty[A]: Product.Metadata[A] = Metadata(None, None, Null.Default)
//
//  def empty[A]: Product[A, Void] = new Product[A, Void](Chain.empty, Chain.empty, Metadata.empty):
//    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Void)] =
//      (openapi, Void).valid
//    override def encode(a: Void): OpenApi.Object = OpenApi.Object.Empty
//
//  def apply[A, B](field: Field[A, B]): Product[A, B] = new Product[A, B](Chain.empty, Chain.one(field), Metadata.empty):
//    override def decodeWithRemainders(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//      field.decode(openapi)
//    override def encode(a: B): OpenApi.Object = field.encode(a, metadata.nulls)
