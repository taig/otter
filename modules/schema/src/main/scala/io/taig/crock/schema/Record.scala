package io.taig.crock.schema

import cats.Eq
import cats.syntax.all.*
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}

sealed abstract class Record[A] extends Schema[A]:
  final override type Self[a] = Record[a]

  def fields: Chain[Field[?, ?]]

//  trait Nulls extends Property[Record.Nulls]
//
//  def nulls: Nulls = ???

  final infix def zip[B](right: Record[B]): Record[(A, B)] = Record.Zip(this, right, Record.Properties.Empty)

  override def optional: Record[Option[A]] = Record.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] =
    Record.Validate(this, validation, g)

object Record extends ToRecordOps:
//  extension [A, B](self: Record[A, B])
//    inline infix def combine(other: Record[A, Unit]): Record[A, B] = other combine self
//    inline infix def combine[C](other: Record[A, C]): Record[A, (B, C)] = self.zip(other)
//    inline def :*(other: Field[A, Unit]): Record[A, B] = combine(other.toRecord)
//    inline def *:(other: Field[A, Unit]): Record[A, B] = combine(other.toRecord)
//    inline def :*[C](other: Field[A, C]): Record[A, (B, C)] = self zip other.toRecord
//    inline def *:[C](other: Field[A, C]): Record[A, (B, C)] = self zip other.toRecord
//
//  extension [A, B <: Tuple](self: Record[A, B])
//    @targetName("appendRecord")
//    inline infix def combine[C](other: Record[A, C]): Record[A, Tuple.Append[B, C]] =
//      self.zip(other).imap { case (b, c) => b :* c }(bc => (bc.init.asInstanceOf[B], bc.last.asInstanceOf[C]))
//    @targetName("appendField")
//    inline def :*[C](other: Field[A, C]): Record[A, Tuple.Append[B, C]] = combine(other.toRecord)
//    @targetName("prependField")
//    inline def *:[C](other: Field[A, C]): Record[A, Tuple.Append[B, C]] = combine(other.toRecord)
//
//  extension [A](self: Record[A, Unit])
//    inline infix def combine[B](other: Record[A, B]): Record[A, B] = self.zip(other).imap { case (_, b) => b }(((), _))
//    inline def :*[B](other: Field[A, B]): Record[A, B] = combine(other.toRecord)
//    inline def *:[B](other: Field[A, B]): Record[A, B] = combine(other.toRecord)

  enum Nulls:
    case Show
    case Hide

  object Nulls:
    val Default: Record.Nulls = Show
    given Eq[Record.Nulls] = Eq.fromUniversalEquals

  final case class Properties[+A](description: Option[String], example: Option[A], nulls: Record.Nulls)

  object Properties:
    val Empty: Record.Properties[Nothing] = Properties(None, None, Nulls.Default)

  final case class Empty(properties: Record.Properties[Unit]) extends Record[Unit]:
    override def fields: Chain[Field[?, ?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[Unit] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class One[A, B](field: Field[A, B], properties: Record.Properties[B]) extends Record[B]:
    override def fields: Chain[Field[A, ?]] = Chain.one(field)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[B] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class Zip[A, B](left: Record[A], right: Record[B], properties: Properties[(A, B)]) extends Record[(A, B)]:
    override def fields: Chain[Field[?, ?]] = left.fields ++ right.fields
    override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[(A, B)] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class Validate[A, B](record: Record[A], validation: Validation[A, B], g: B => A) extends Record[B]:
    export record.{fields, isOptional}
    override def constraints: Chain[Constraint] = record.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(record, _.description, value => copy(record = record.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(record, _.example, value => copy(record = record.example(value)), validation, g)

  final case class Optional[A](self: Record[A]) extends Record[Option[A]]:
    export self.{constraints, fields}
    override def isOptional: Boolean = true
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  val empty: Record[Unit] = Empty(Properties.Empty)
  def apply[A, B](field: Field[A, B]): Record[B] = One(field, Properties.Empty)

final class RecordOps[A](self: Record[A]) extends AnyVal:
  inline def :*[B, C](other: Field[B, C]): Record[(A, C)] = self.zip(other.toRecord)
  inline def *:[B, C](other: Field[B, C]): Record[(C, A)] = other.toRecord.zip(self)
  inline def :*[B](other: Field[B, Unit]): Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
  inline def *:[B](other: Field[B, Unit]): Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))
final class RecordOpsUnit(self: Record[Unit]) extends AnyVal:
  inline def :*[A, B](other: Field[A, B]): Record[B] = self.zip(other.toRecord).imap { case (_, b) => b }(((), _))
  inline def *:[A, B](other: Field[A, B]): Record[B] = other.toRecord.zip(self).imap { case (b, _) => b }((_, ()))
final class RecordOpsTuple[A <: Tuple](self: Record[A]) extends AnyVal:
  inline def :*[B, C](other: Field[B, C]): Record[Tuple.Append[A, C]] =
    self.zip(other.toRecord).imap { case (a, c) => a :* c }(ac => (ac.init.asInstanceOf[A], ac.last.asInstanceOf[C]))
  inline def *:[B, C](other: Field[B, C]): Record[C *: A] =
    other.toRecord.zip(self).imap { case (c, a) => c *: a } { case c *: a => (c, a) }
  inline def :*[B](other: Field[B, Unit]): Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
  inline def *:[B](other: Field[B, Unit]): Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))

trait ToRecordOps extends ToRecordOps1:
  implicit def toRecordOpsUnit(self: Record[Unit]): RecordOpsUnit = new RecordOpsUnit(self)
  implicit def toRecordOpsTuple[A <: Tuple](self: Record[A]): RecordOpsTuple[A] = new RecordOpsTuple(self)
trait ToRecordOps1:
  implicit def toRecordOps[A](self: Record[A]): RecordOps[A] = new RecordOps(self)
