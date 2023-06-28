package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import cats.{Eq, Eval}
import io.taig.openapi.validation.Constraint
import io.taig.openapi.{History, OpenApi}

sealed abstract class Field[A, B]:
  self =>

  def default: Option[B]
  def modifyDefault(f: Option[B] => Option[B]): Field[A, B]

  def key: Eval[Schema.Value[A]]

  def name: A
  final def renderName: String = key.value.render(name)

  def nulls: Field.Null
  def modifyNulls(f: Field.Null => Field.Null): Field[A, B]
  final def withNulls(value: Field.Null): Field[A, B] = modifyNulls(_ => value)
  final def hideNulls: Field[A, B] = withNulls(Field.Null.Hide)
  final def showNulls: Field[A, B] = withNulls(Field.Null.Show)
  final def inheritNulls: Field[A, B] = withNulls(Field.Null.Inherit)

  def schema: Eval[Schema[?]]

  final def optional: Field[A, Option[B]] = Field.Optional(this)

  final transparent inline infix def zip[C](field: Field[A, C]): Product[A, ?] = toProduct zip field.toProduct
  final transparent inline infix def :*[C](field: Field[A, C]): Product[A, ?] = toProduct :* field

  final def toProduct: Product[A, B] = Product(this)
  final def to[C](using Evidence.Product.Aux[C, B]): Product[A, C] = toProduct.to[C]

  def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)]

  def encode(b: B, parent: Product.Nulls): OpenApi.Object

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    val Default: Field.Null = Null.Inherit

    given Eq[Null] = Eq.fromUniversalEquals

  final private case class Required[A, B](
      default: Option[B],
      key: Eval[Schema.Value[A]],
      name: A,
      nulls: Field.Null,
      schema: Eval[Schema[B]]
  ) extends Field[A, B]:
    override def modifyDefault(f: Option[B] => Option[B]): Field[A, B] = copy(default = f(default))
    override def modifyNulls(f: Field.Null => Field.Null): Field[A, B] = copy(nulls = f(nulls))
    override def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
      val name = renderName
      openapi.get(name) match
        case Some(value) =>
          schema.value
            .decode(value)
            .bimap(_.modifyHistory(name /: _), (openapi.remove(name), _))
        case None =>
          val violation = Constraint.required.toViolation(actual = OpenApi.Null)
          val violations = Violations.oneNec(History.Root / name, violation)
          default.toValid(violations).tupleLeft(openapi)
    override def encode(b: B, parent: Product.Nulls): OpenApi.Object =
      OpenApi.Object.one(renderName, schema.value.encode(b))

  final private case class Optional[A, B](field: Field[A, B]) extends Field[A, Option[B]]:
    export field.{key, name, nulls, schema}
    override def default: Option[Option[B]] = field.default.map(_.some)
    override def modifyDefault(f: Option[Option[B]] => Option[Option[B]]): Field[A, Option[B]] =
      copy(field.modifyDefault(value => f(value.map(_.some)).flatten))
    override def modifyNulls(f: Null => Null): Field[A, Option[B]] = copy(field.modifyNulls(f))
    override def decode(openapi: OpenApi.Object): Validated[Violations, (OpenApi.Object, Option[B])] =
      if openapi.contains(renderName)
      then field.decode(openapi).map(_.map(_.some))
      else (openapi, none[B]).valid
    override def encode(b: Option[B], parent: Product.Nulls): OpenApi.Object = b match
      case Some(b) => field.encode(b, parent)
      case None =>
        val dropNull = (field.nulls, parent) match
          case (Field.Null.Inherit, Product.Nulls.Hide) | (Field.Null.Hide, _) => true
          case _                                                               => false

        if dropNull then OpenApi.Object.Empty else OpenApi.Object.one(field.renderName, OpenApi.Null)

  def apply[A, B](name: A, key: Eval[Schema.Value[A]], schema: Eval[Schema[B]]): Field[A, B] =
    Required[A, B](none, key, name, Null.Default, schema)
