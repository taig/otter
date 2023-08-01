//package io.taig.crock.schema
//
//import cats.Eq
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.crock.{Encoder, OpenApi}
//import io.taig.crock.syntax.*
//import io.taig.crock.validation.{Constraint, Validation}
//
//sealed abstract class Product[A, B] extends Schema[B]:
//  self =>
//
//  final override type Codec = OpenApi.Object
//  final override type Self[a] = Product[A, a]
//
//  def fields: Chain[Field[A, ?]]
//
//  def nulls: Product.Nulls
//  def modifyNulls(f: Product.Nulls => Product.Nulls): Product[A, B]
//  final def withNulls(nulls: Product.Nulls): Product[A, B] = modifyNulls(_ => nulls)
//  final def showNulls: Product[A, B] = withNulls(Product.Nulls.Show)
//  final def hideNulls: Product[A, B] = withNulls(Product.Nulls.Hide)
//
//  final def product[C](right: Product[A, C]): Product[A, (B, C)] = Product.Zip(this, right, none, none, nulls)
//
//  final transparent inline infix def zip[C](product: Product[A, C]): Product[A, ?] = inline (this, product) match
//    case (b: Product[A, Unit], c: Product[A, C]) => b.product(c).imap[C] { case (_, c) => c }(c => ((), c))
//    case (b: Product[A, B], c: Product[A, Unit]) => b.product(c).imap[B] { case (c, _) => c }(c => (c, ()))
//    case (a: Product[A, Tuple], b) =>
//      a.product(b).imap[Tuple.Append[B, C]] { case (b, c) => b :* c }(bc => (bc.init, bc.last.asInstanceOf[C]))
//    case (b, c) => b.product(c)
//
//  final transparent inline def :*[C](field: Field[A, C]): Product[A, ?] = this zip field.toProduct
//
//  final def to[C](using evidence: Evidence.Product.Aux[C, B]): Product[A, C] = imap(evidence.from)(evidence.to)
//
//  final override def ivalidate[C: Encoder, D](validation: Validation[C, B, B, D])(g: D => B): Product[A, D] =
//    Product.Validate(this, validation, g)
//
//  final override def decode(crock: OpenApi): Validated[Violations, B] = crock match
//    case crock: OpenApi.Object => decodeWithRemainders(crock).map(_._2)
//    case _                       => typeViolations("Object", crock).invalid
//
//  def decodeWithRemainders(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)]
//
//object Product:
//  final private case class Empty[A](description: Option[String], example: Option[Unit], nulls: Nulls)
//      extends Product[A, Unit]:
//    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
//    override def fields: Chain[Field[A, ?]] = Chain.empty
//    override def modifyDescription(f: Option[String] => Option[String]): Product[A, Unit] =
//      copy(description = f(description))
//    override def modifyExample(f: Option[Unit] => Option[Unit]): Product[A, Unit] = copy(example = f(example))
//    override def modifyNulls(f: Nulls => Nulls): Product[A, Unit] = copy(nulls = f(nulls))
//
//    override def decodeWithRemainders(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, Unit)] =
//      (crock, ()).valid
//    override def encode(a: Unit): OpenApi.Object = OpenApi.Object.Empty
//
//  final private case class Root[A, B](
//      description: Option[String],
//      example: Option[B],
//      field: Field[A, B],
//      nulls: Nulls
//  ) extends Product[A, B]:
//    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
//    override def fields: Chain[Field[A, ?]] = Chain.one(field)
//    override def modifyDescription(f: Option[String] => Option[String]): Product[A, B] =
//      copy(description = f(description))
//    override def modifyExample(f: Option[B] => Option[B]): Product[A, B] = copy(example = f(example))
//    override def modifyNulls(f: Nulls => Nulls): Product[A, B] = copy(nulls = f(nulls))
//    override def decodeWithRemainders(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//      field.decode(crock)
//    override def encode(b: B): OpenApi.Object = field.encode(b, nulls)
//
//  final private case class Validate[A, B, C: Encoder, D](
//      product: Product[A, B],
//      validation: Validation[C, B, B, D],
//      g: D => B
//  ) extends Product[A, D]:
//    override def constraints: Chain[Constraint[OpenApi]] =
//      product.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
//    override def description: Option[String] = product.description
//    override def example: Option[D] = product.example.flatMap(validation.run(_).toOption)
//    override def fields: Chain[Field[A, ?]] = product.fields
//    override def nulls: Nulls = product.nulls
//    override def modifyDescription(f: Option[String] => Option[String]): Product[A, D] =
//      copy(product = product.modifyDescription(f))
//    override def modifyExample(f: Option[D] => Option[D]): Product[A, D] =
//      copy(product = product.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
//    override def modifyNulls(f: Nulls => Nulls): Product[A, D] = copy(product = product.modifyNulls(f))
//    override def decodeWithRemainders(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, D)] =
//      product.decodeWithRemainders(crock).andThen(_.traverse(applyValidation(validation, product.encode)))
//    override def encode(c: D): OpenApi.Object = product.encode(g(c))
//
//  final private case class Zip[A, B, C](
//      left: Product[A, B],
//      right: Product[A, C],
//      description: Option[String],
//      example: Option[(B, C)],
//      nulls: Nulls
//  ) extends Product[A, (B, C)]:
//    override def constraints: Chain[Constraint[OpenApi]] = left.constraints ++ right.constraints
//    override def fields: Chain[Field[A, ?]] = left.fields ++ right.fields
//    override def modifyDescription(f: Option[String] => Option[String]): Product[A, (B, C)] =
//      copy(description = f(description))
//    override def modifyExample(f: Option[(B, C)] => Option[(B, C)]): Product[A, (B, C)] = copy(example = f(example))
//    override def modifyNulls(f: Nulls => Nulls): Product[A, (B, C)] = copy(nulls = f(nulls))
//
//    override def decodeWithRemainders(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, (B, C))] =
//      left.decodeWithRemainders(crock) match
//        case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
//        case Validated.Invalid(violations) =>
//          left.decodeWithRemainders(crock).fold(violations merge _, _ => violations).invalid
//
//    override def encode(ab: (B, C)): OpenApi.Object = left.encode(ab._1) ++ right.encode(ab._2)
//
//  enum Nulls:
//    case Show
//    case Hide
//
//  object Nulls:
//    val Default: Product.Nulls = Show
//
//    given Eq[Product.Nulls] = Eq.fromUniversalEquals
//
//  def empty[A]: Product[A, Unit] = Empty(none, none, Nulls.Default)
//
//  def apply[A, B](field: Field[A, B]): Product[A, B] = Root(none, none, field, Nulls.Default)
