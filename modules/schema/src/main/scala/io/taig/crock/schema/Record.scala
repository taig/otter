package io.taig.crock.schema

import cats.Eq
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}
import monocle.syntax.all.*

sealed abstract class Record[A, B] extends Schema[B]:
  self =>

  final override type Self[a] = Record[A, a]

  def fields: Chain[Field[A, ?]]

  final override def ivalidate[C](validation: Validation[B, C])(g: C => B): Record[A, C] =
    Record.Validate(this, validation, g)

  final def product[C](right: Record[A, C]): Record[A, (B, C)] = Record.Zip(this, right, Record.Properties.Empty)

  final transparent inline infix def zip[C](right: Record[A, C]): Record[A, ?] = inline (this, right) match
    case (b: Record[A, Unit], c: Record[A, C]) => b.product(c).imap[C] { case (_, c) => c }(c => ((), c))
    case (b: Record[A, B], c: Record[A, Unit]) => b.product(c).imap[B] { case (c, _) => c }(c => (c, ()))
    case (a: Record[A, Tuple], b) =>
      a.product(b).imap[Tuple.Append[B, C]] { case (b, c) => b :* c }(bc => (bc.init, bc.last.asInstanceOf[C]))
    case (b, c) => b.product(c)

  final transparent inline def :*[C](field: Field[A, C]): Record[A, ?] = this zip field.toRecord
  final transparent inline def *:[C](field: Field[A, C]): Record[A, ?] = field.toRecord zip this

  final def to[C](using evidence: Evidence.Product.Aux[C, B]): Record[A, C] = imap(evidence.from)(evidence.to)

object Record:
  enum Nulls:
    case Show
    case Hide

  object Nulls:
    val Default: Record.Nulls = Show

    given Eq[Record.Nulls] = Eq.fromUniversalEquals

  final case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Record.Properties[Nothing] = Properties(None, None)

  final case class Empty[A](properties: Record.Properties[Unit]) extends Record[A, Unit]:
    override def fields: Chain[Field[A, ?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[String] =
      Property.Optional(properties.description, this.focus(_.properties.description).modify)
    override def example: Property.Optional[Unit] =
      Property.Optional(properties.example, this.focus(_.properties.example).modify)

  final case class One[A, B](field: Field[A, B], properties: Record.Properties[B]) extends Record[A, B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def fields: Chain[Field[A, ?]] = Chain.one(field)
    override def description: Property.Optional[String] =
      Property.Optional(properties.description, this.focus(_.properties.description).modify)
    override def example: Property.Optional[B] =
      Property.Optional(properties.example, this.focus(_.properties.example).modify)

  final case class Zip[A, B, C](left: Record[A, B], right: Record[A, C], properties: Properties[(B, C)])
      extends Record[A, (B, C)]:
    override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
    override def fields: Chain[Field[A, ?]] = left.fields ++ right.fields
    override def description: Property.Optional[String] =
      Property.Optional(properties.description, this.focus(_.properties.description).modify)
    override def example: Property.Optional[(B, C)] =
      Property.Optional(properties.example, this.focus(_.properties.example).modify)

  final case class Validate[A, B, C](record: Record[A, B], validation: Validation[B, C], g: C => B)
      extends Record[A, C]:
    export record.fields
    override def constraints: Chain[Constraint] = record.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(record, _.description, value => copy(record = record.description(value)))
    override def example: Property.Optional[C] =
      Property.Optional(record, _.example, value => copy(record = record.example(value)), validation, g)

  def empty[A]: Record[A, Unit] = Empty(Properties.Empty)
  def apply[A, B](field: Field[A, B]): Record[A, B] = One(field, Properties.Empty)
