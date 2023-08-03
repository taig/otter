package io.taig.crock.schema

import cats.Eq
import cats.syntax.all.*
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}

sealed abstract class Record[A] extends Schema[A]:
  final override type Self[a] = Record[a]

  def fields: Chain[Field[?, ?]]

  trait Nulls extends Property[Record.Null]:
    final def show: Record[A] = apply(Record.Null.Show)
    final def hide: Record[A] = apply(Record.Null.Hide)

  object Nulls:
    def apply(a: Record.Null, g: (Record.Null => Record.Null) => Record[A]): Nulls = new Nulls:
      override def value: Record.Null = a
      override def modify(f: Record.Null => Record.Null): Record[A] = g(f)

  def nulls: Nulls

  final infix def zip[B](right: Record[B]): Record[(A, B)] = Record.Zip(this, right, Record.Properties.Empty)

  override def optional: Record[Option[A]] = Record.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] =
    Record.Validate(this, validation, g)
  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

object Record extends ToRecordOps:
  enum Null:
    case Show
    case Hide

  object Null:
    val Default: Record.Null = Show
    given Eq[Record.Null] = Eq.fromUniversalEquals

  final case class Properties[+A](description: Option[String], example: Option[A], nulls: Record.Null)

  object Properties:
    val Empty: Record.Properties[Nothing] = Properties(None, None, Null.Default)

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
    override def nulls: Nulls = Nulls(
      properties.nulls,
      f => copy(properties = properties.copy(nulls = f(properties.nulls)))
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
    override def nulls: Nulls = Nulls(
      properties.nulls,
      f => copy(properties = properties.copy(nulls = f(properties.nulls)))
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
    override def nulls: Nulls = Nulls(
      properties.nulls,
      f => copy(properties = properties.copy(nulls = f(properties.nulls)))
    )

  final case class Validate[A, B](self: Record[A], validation: Validation[A, B], g: B => A) extends Record[B]:
    export self.{fields, isOptional}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)
    override def nulls: Nulls = Nulls(self.nulls.value, f => copy(self = self.nulls.modify(f)))

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
    override def nulls: Nulls = Nulls(self.nulls.value, f => copy(self = self.nulls.modify(f)))

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
