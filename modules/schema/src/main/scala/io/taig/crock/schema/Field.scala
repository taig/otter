package io.taig.crock.schema

import cats.syntax.all.*
import cats.{Eq, Eval}

import scala.annotation.targetName
import monocle.syntax.all.*

sealed abstract class Field[A, B]:
  self =>

  trait Property[C]:
    def value: C
    def modify(f: C => C): Field[A, B]
    final def apply(value: C): Field[A, B] = modify(_ => value)

  object Property:
    trait Optional[C] extends Property[Option[C]]:
      @targetName("as")
      def apply(value: C): Field[A, B] = apply(Some(value))
      def clear: Field[A, B] = apply(None)

    object Optional:
      def apply[C](c: Option[C], g: (Option[C] => Option[C]) => Field[A, B]): Property.Optional[C] = new Optional[C]:
        override def value: Option[C] = c
        override def modify(f: Option[C] => Option[C]): Field[A, B] = g(f)

    trait Nulls extends Property[Field.Null]:
      def inherit: Field[A, B] = apply(Field.Null.Inherit)
      def hide: Field[A, B] = apply(Field.Null.Hide)
      def show: Field[A, B] = apply(Field.Null.Show)

    object Nulls:
      def apply(nulls: Field.Null, g: (Field.Null => Field.Null) => Field[A, B]): Property.Nulls = new Nulls:
        override def value: Field.Null = nulls
        override def modify(f: Field.Null => Field.Null): Field[A, B] = g(f)

  def name: A
  def key: Eval[Schema.Value[A]]

  def default: Property.Optional[B]
  def nulls: Property.Nulls

  def schema: Eval[Schema[?]]

  final def optional: Field[A, Option[B]] = Field.Optional(this)

//  final transparent inline infix def zip[C](field: Field[A, C]): Product[A, ?] = toProduct zip field.toProduct
//  final transparent inline infix def :*[C](field: Field[A, C]): Product[A, ?] = toProduct :* field

//  final def toProduct: Product[A, B] = Product(this)
//  final def to[C](using Evidence.Product.Aux[C, B]): Product[A, C] = toProduct.to[C]

object Field:
  enum Null:
    case Hide
    case Inherit
    case Show

  object Null:
    val Default: Field.Null = Null.Inherit

    given Eq[Null] = Eq.fromUniversalEquals

  final case class Properties[+A](default: Option[A], nulls: Field.Null)

  object Properties:
    val Empty: Field.Properties[Nothing] = Properties(None, Field.Null.Default)

  final private case class Required[A, B](
      key: Eval[Schema.Value[A]],
      name: A,
      schema: Eval[Schema[B]],
      properties: Field.Properties[B]
  ) extends Field[A, B]:
    override def default: Property.Optional[B] =
      Property.Optional(properties.default, this.focus(_.properties.default).modify)
    override def nulls: Property.Nulls =
      Property.Nulls(properties.nulls, this.focus(_.properties.nulls).modify)

//    override def decode(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//      val name = renderName
//      crock.get(name) match
//        case Some(value) =>
//          schema.value
//            .decode(value)
//            .bimap(_.modifyHistory(name /: _), (crock.remove(name), _))
//        case None =>
//          val violation = Constraint.required.toViolation(actual = OpenApi.Null)
//          val violations = Violations.oneNec(History.Root / name, violation)
//          default.toValid(violations).tupleLeft(crock)
//    override def encode(b: B, parent: Product.Nulls): OpenApi.Object =
//      OpenApi.Object.one(renderName, schema.value.encode(b))
//
  final private case class Optional[A, B](field: Field[A, B]) extends Field[A, Option[B]]:
    export field.{key, name, schema}
    override def default: Property.Optional[Option[B]] = Property.Optional(
      field.default.value.map(_.some),
      f => copy(field = field.default.modify(value => f(value.map(_.some)).flatten))
    )
    override def nulls: Property.Nulls = Property.Nulls(field.nulls.value, f => copy(field = field.nulls.modify(f)))

//    override def decode(crock: OpenApi.Object): Validated[Violations, (OpenApi.Object, Option[B])] =
//      if crock.contains(renderName)
//      then field.decode(crock).map(_.map(_.some))
//      else (crock, none[B]).valid
//    override def encode(b: Option[B], parent: Product.Nulls): OpenApi.Object = b match
//      case Some(b) => field.encode(b, parent)
//      case None =>
//        val dropNull = (field.nulls, parent) match
//          case (Field.Null.Inherit, Product.Nulls.Hide) | (Field.Null.Hide, _) => true
//          case _                                                               => false
//
//        if dropNull then OpenApi.Object.Empty else OpenApi.Object.one(field.renderName, OpenApi.Null)

  def apply[A, B](name: A, key: Eval[Schema.Value[A]], schema: Eval[Schema[B]]): Field[A, B] =
    Required[A, B](key, name, schema, Properties.Empty)
