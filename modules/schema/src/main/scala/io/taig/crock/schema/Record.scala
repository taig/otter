package io.taig.crock.schema

import cats.Eq
import cats.syntax.all.*
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}

import scala.annotation.targetName

sealed abstract class Record[A, B] extends Schema[B]:
  self =>

  final override type Self[a] = Record[A, a]

  def fields: Chain[Field[A, ?]]

  final infix def zip[C](right: Record[A, C]): Record[A, (B, C)] = Record.Zip(this, right, Record.Properties.Empty)

  override def optional: Record[A, Option[B]] = ???

  final override def ivalidate[C](validation: Validation[B, C])(g: C => B): Record[A, C] =
    Record.Validate(this, validation, g)

  final def to[C](using evidence: Evidence.Product.Aux[C, B]): Record[A, C] = imap(evidence.from)(evidence.to)

object Record:
  extension [A, B](self: Record[A, B])
    inline infix def combine(other: Record[A, Unit]): Record[A, B] = other combine self
    inline infix def combine[C](other: Record[A, C]): Record[A, (B, C)] = self.zip(other)
    inline def :*(other: Field[A, Unit]): Record[A, B] = combine(other.toRecord)
    inline def *:(other: Field[A, Unit]): Record[A, B] = combine(other.toRecord)
    inline def :*[C](other: Field[A, C]): Record[A, (B, C)] = self zip other.toRecord
    inline def *:[C](other: Field[A, C]): Record[A, (B, C)] = self zip other.toRecord

  extension [A, B <: Tuple](self: Record[A, B])
    @targetName("appendRecord")
    inline infix def combine[C](other: Record[A, C]): Record[A, Tuple.Append[B, C]] =
      self.zip(other).imap { case (b, c) => b :* c }(bc => (bc.init.asInstanceOf[B], bc.last.asInstanceOf[C]))
    @targetName("appendField")
    inline def :*[C](other: Field[A, C]): Record[A, Tuple.Append[B, C]] = combine(other.toRecord)
    @targetName("prependField")
    inline def *:[C](other: Field[A, C]): Record[A, Tuple.Append[B, C]] = combine(other.toRecord)

  extension [A](self: Record[A, Unit])
    inline infix def combine[B](other: Record[A, B]): Record[A, B] = self.zip(other).imap { case (_, b) => b }(((), _))
    inline def :*[B](other: Field[A, B]): Record[A, B] = combine(other.toRecord)
    inline def *:[B](other: Field[A, B]): Record[A, B] = combine(other.toRecord)

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
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[Unit] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class One[A, B](field: Field[A, B], properties: Record.Properties[B]) extends Record[A, B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def fields: Chain[Field[A, ?]] = Chain.one(field)
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[B] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class Zip[A, B, C](left: Record[A, B], right: Record[A, C], properties: Properties[(B, C)])
      extends Record[A, (B, C)]:
    override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
    override def fields: Chain[Field[A, ?]] = left.fields ++ right.fields
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[(B, C)] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class Optional[A, B](self: Record[A, B]) extends Record[A, Option[B]]:
    export self.{constraints, fields}
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[B]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

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
