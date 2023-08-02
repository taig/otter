package io.taig.crock.schema

import cats.syntax.all.*
import cats.{Eq, Eval}

final case class Field[A, B](
    key: Eval[Schema.Value[A]],
    name: A,
    schema: Eval[Schema[B]],
    properties: Field.Properties[B]
):
  trait Property[C]:
    def value: C
    def modify(f: C => C): Field[A, B]
    final def apply(value: C): Field[A, B] = modify(_ => value)

  object Property:
    trait Nulls extends Property[Field.Null]:
      def inherit: Field[A, B] = apply(Field.Null.Inherit)
      def hide: Field[A, B] = apply(Field.Null.Hide)
      def show: Field[A, B] = apply(Field.Null.Show)

    object Nulls:
      def apply(nulls: Field.Null, g: (Field.Null => Field.Null) => Field[A, B]): Property.Nulls = new Nulls:
        override def value: Field.Null = nulls
        override def modify(f: Field.Null => Field.Null): Field[A, B] = g(f)

  def nulls: Property.Nulls = Property.Nulls(
    properties.nulls,
    f => copy(properties = properties.copy(nulls = f(properties.nulls)))
  )

  def toRecord: Record[B] = Record(this)
  def to[C](using Evidence.Product.Aux[C, B]): Record[C] = toRecord.to[C]

object Field extends ToFieldOps:
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

//  final private case class Required[A, B](
//      key: Eval[Schema.Value[A]],
//      name: A,
//      schema: Eval[Schema[B]],
//      properties: Field.Properties[B]
//  ) extends Field[A, B]:
//    override def name[C](encoder: Encoder[Schema.Value, C]): C = encoder.encode(key.value, name)
//    override def default: Property.Optional[B] = Property.Optional(
//      properties.default,
//      f => copy(properties = properties.copy(default = f(properties.default)))
//    )
//    override def nulls: Property.Nulls = Property.Nulls(
//      properties.nulls,
//      f => copy(properties = properties.copy(nulls = f(properties.nulls)))
//    )

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
    Field[A, B](key, name, schema, Properties.Empty)

final class FieldOps[A, B](self: Field[A, B]) extends AnyVal:
  inline def :*[C, D](other: Field[C, D]): Record[(B, D)] = self.toRecord :* other
  inline def *:[C, D](other: Field[C, D]): Record[(D, B)] = other *: self.toRecord
  inline def :*[C](other: Field[C, Unit]): Record[B] = self.toRecord :* other
  inline def *:[C](other: Field[C, Unit]): Record[B] = other *: self.toRecord
final class FieldOpsUnit[A](self: Field[A, Unit]) extends AnyVal:
  inline def :*[B, C](other: Field[B, C]): Record[C] = self.toRecord :* other
  inline def *:[B, C](other: Field[B, C]): Record[C] = other *: self.toRecord

trait ToFieldOps extends ToFieldOps1:
  implicit final def toFieldOpsUnit[A](self: Field[A, Unit]): FieldOpsUnit[A] = new FieldOpsUnit(self)
trait ToFieldOps1:
  implicit final def toFieldOps[A, B](self: Field[A, B]): FieldOps[A, B] = new FieldOps(self)
