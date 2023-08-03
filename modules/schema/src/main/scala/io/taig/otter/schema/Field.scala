package io.taig.otter.schema

import cats.syntax.all.*
import cats.Eval

final case class Field[A, B](
    key: Eval[Schema.Value[A]],
    name: A,
    schema: Eval[Schema[B]],
    properties: Field.Properties[B]
):
  trait Nulls:
    def value: Option[Null]
    def modify(f: Option[Null] => Option[Null]): Field[A, B]
    final def apply(value: Option[Null]): Field[A, B] = modify(_ => value)
    def inherit: Field[A, B] = apply(None)
    def hide: Field[A, B] = apply(Some(Null.Hide))
    def show: Field[A, B] = apply(Some(Null.Show))

  object Nulls:
    def apply(nulls: Option[Null], g: (Option[Null] => Option[Null]) => Field[A, B]): Nulls = new Nulls:
      override def value: Option[Null] = nulls
      override def modify(f: Option[Null] => Option[Null]): Field[A, B] = g(f)

  def nulls: Nulls = Nulls(
    properties.nulls,
    f => copy(properties = properties.copy(nulls = f(properties.nulls)))
  )

  def toRecord: Record[B] = Schema.Record(this)
  def to[C](using Evidence.Product.Aux[C, B]): Record[C] = toRecord.to[C]

object Field extends ToFieldOps:
  final case class Properties[+A](default: Option[A], nulls: Option[Null])

  object Properties:
    val Empty: Field.Properties[Nothing] = Properties(None, None)

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

//    override def decode(otter: OpenApi.Object): Validated[Violations, (OpenApi.Object, B)] =
//      val name = renderName
//      otter.get(name) match
//        case Some(value) =>
//          schema.value
//            .decode(value)
//            .bimap(_.modifyHistory(name /: _), (otter.remove(name), _))
//        case None =>
//          val violation = Constraint.required.toViolation(actual = OpenApi.Null)
//          val violations = Violations.oneNec(History.Root / name, violation)
//          default.toValid(violations).tupleLeft(otter)
//    override def encode(b: B, parent: Product.Nulls): OpenApi.Object =
//      OpenApi.Object.one(renderName, schema.value.encode(b))
//
//    override def decode(otter: OpenApi.Object): Validated[Violations, (OpenApi.Object, Option[B])] =
//      if otter.contains(renderName)
//      then field.decode(otter).map(_.map(_.some))
//      else (otter, none[B]).valid
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
