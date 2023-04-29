//package io.taig.openapi.schema
//
//import cats.data.Validated
//import cats.syntax.all.*
//import cats.{Eq, Eval}
//import io.taig.openapi.OpenApi
//import io.taig.validation.{Constraint, Violation}
//
//sealed abstract class Field[A, B]:
//  self =>
//
//  def default: Option[B]
//
//  def key: Eval[Value[A]]
//
//  def name: A
//  final def renderName: String = key.value.render(name)
//
//  def nulls: Field.Null
//  def modifyNulls(f: Field.Null => Field.Null): Field[A, B]
//  final def withNulls(value: Field.Null): Field[A, B] = modifyNulls(_ => value)
//  final def hideNulls: Field[A, B] = withNulls(Field.Null.Hide)
//  final def showNulls: Field[A, B] = withNulls(Field.Null.Show)
//  final def inheritNulls: Field[A, B] = withNulls(Field.Null.Inherit)
//
//  def schema: Eval[Schema[?]]
//
//  final def optional: Field[A, Option[B]] = Field.Optional(this)
//
//  // TODO imap, ivalidate, ...
//
//  final transparent inline infix def zip[C](field: Field[A, C]): Product[A, ?] = toProduct zip field.toProduct
//  final transparent inline infix def :*[C](field: Field[A, C]): Product[A, ?] = toProduct :* field
//
//  final def toProduct: Product[A, B] = Product(this)
//
//  final def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//    Field.decode(this)(openapi)
//
//  final def encode(b: B, parent: Product.Null): OpenApi.Object = Field.encode(this)(b, parent)
//
//object Field:
//  enum Null:
//    case Hide
//    case Inherit
//    case Show
//
//  object Null:
//    val Default: Field.Null = Null.Inherit
//
//    given Eq[Null] = Eq.fromUniversalEquals
//
//  final case class Required[A, B](
//      default: Option[B],
//      key: Eval[Value[A]],
//      name: A,
//      nulls: Field.Null,
//      schema: Eval[Schema[B]]
//  ) extends Field[A, B]:
//    override def modifyNulls(f: Field.Null => Field.Null): Field[A, B] = copy(nulls = f(nulls))
//
//  object Required:
//    def decode[A, B](field: Field.Required[A, B])(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//      val name = field.renderName
//
//      openapi.get(name) match
//        case Some(value) =>
//          field.schema.value
//            .decode(value)
//            .bimap(_.modifyHistory(name /: _), (openapi.remove(name), _))
//        case None =>
//          val constraint = Constraint("required", reference = none)
//          val violations = Violations.rootNec(Violation(constraint, actual = OpenApi.Null))
//          field.default.toValid(violations).tupleLeft(openapi)
//
//    def encode[A, B](field: Field.Required[A, B])(b: B): OpenApi.Object =
//      OpenApi.Object.one(field.renderName, field.schema.value.encode(b))
//
//  final case class Optional[A, B](field: Field[A, B]) extends Field[A, Option[B]]:
//    export field.{key, name, nulls, schema}
//    override def default: Option[Option[B]] = field.default.map(_.some)
//    override def modifyNulls(f: Null => Null): Field[A, Option[B]] = copy(field.modifyNulls(f))
//
//  object Optional:
//    def decode[A, B](field: Field.Optional[A, B])(
//        openapi: OpenApi.Object
//    ): Validated[Violations, (OpenApi.Object, Option[B])] =
//      if openapi.contains(field.renderName)
//      then field.field.decode(openapi).map(_.map(_.some))
//      else (openapi, none[B]).valid
//
//    def encode[A, B](field: Field.Optional[A, B])(b: Option[B], parent: Product.Null): OpenApi.Object = b match
//      case Some(b) => field.field.encode(b, parent)
//      case None =>
//        val dropNull = (field.nulls, parent) match
//          case (Field.Null.Inherit, Product.Null.Hide) | (Field.Null.Hide, _) => true
//          case _                                                              => false
//
//        if dropNull then OpenApi.Object.Empty else OpenApi.Object.one(field.renderName, OpenApi.Null)
//
//  def decode[A, B](field: Field[A, B])(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//    field match
//      case field: Required[?, ?] => Required.decode(field)(openapi)
//      case field: Optional[?, ?] => Optional.decode(field)(openapi)
//
//  def encode[A, B](field: Field[A, B])(b: B, parent: Product.Null): OpenApi.Object = field match
//    case field: Required[?, ?] => Required.encode(field)(b)
//    case field: Optional[?, ?] => Optional.encode(field)(b, parent)
//
//  def apply[A, B](name: A, key: Eval[Value[A]], schema: Eval[Schema[B]]): Field[A, B] =
//    Required[A, B](none, key, name, Null.Default, schema)
