package io.taig.otter

import cats.syntax.all.*
import cats.data.Validated
import cats.Id as Identity
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

sealed abstract class Fields[+O <: Data, A]:
  self =>

  def toVector: Vector[Field[?, ?]]

  final def imap[B](f: A => B)(g: B => A): Fields[O, B] = new Fields[O, B]:
    export self.toVector
    override def decodeArray(data: Vector[Data]): Codec.Result[B] = self.decodeArray(data).map(f)
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[B] = self.decodeRecord(data).map(f)
    override def encodeArray(b: B): Vector[O] = self.encodeArray(g(b))
    override def encodeRecord(b: B, nulls: Null): Vector[(String, O)] = self.encodeRecord(g(b), nulls)

  final def zip[P <: Data, B](fields: Fields[P, B]): Fields[O | P, (A, B)] = new Fields[O | P, (A, B)]:
    override def toVector: Vector[Field[?, ?]] = self.toVector ++ fields.toVector
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[((A, B))] =
      val (left, remainders) = data.filterKeys(self.toVector.map(_.name))
      val (right, _) = remainders.filterKeys(fields.toVector.map(_.name))
      (self.decodeRecord(left), fields.decodeRecord(right)).tupled
    override def decodeArray(data: Vector[Data]): Codec.Result[(A, B)] =
      val (left, right) = data.splitAt(self.toVector.length)

      self.decodeArray(left) match
        case Validated.Valid(a) => fields.decodeArray(right).tupleLeft(a)
        case Validated.Invalid(violations) =>
          fields.decodeArray(right).fold(violations.combine, _ => violations).invalid
    override def encodeRecord(ab: (A, B), nulls: Null): Vector[(String, O | P)] =
      self.encodeRecord(ab._1, nulls) ++ fields.encodeRecord(ab._2, nulls)
    override def encodeArray(ab: (A, B)): Vector[O | P] = self.encodeArray(ab._1) ++ fields.encodeArray(ab._2)

  final def :*[P <: Data, B](field: Field[P, B])(using merge: Evidence.Merge[A, B]): Fields[O | P, merge.Out] =
    zip(field.toFields).imap(merge.apply)(merge.unapply)

  final def *:[P <: Data, B](field: Field[P, B])(using merge: Evidence.Merge[B, A]): Fields[P | O, merge.Out] =
    field.toFields.zip(this).imap(merge.apply)(merge.unapply)

  final def toTuple: Tuple[Identity, O, A] = Tuple(this)
  final def toRecord: Record[Identity, O, A] = Record(this)

  def decodeRecord(data: Vector[(String, Data)]): Codec.Result[A]

  def decodeArray(data: Vector[Data]): Codec.Result[A]

  def encodeRecord(a: A, nulls: Null): Vector[(String, O)]

  def encodeArray(a: A): Vector[O]

object Fields:
  val Empty: Fields[Nothing, Unit] = new Fields[Nothing, Unit]:
    override def toVector: Vector[Nothing] = Vector.empty
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[Unit] = ().valid
    override def decodeArray(data: Vector[Data]): Codec.Result[Unit] = ().valid
    override def encodeRecord(a: Unit, nulls: Null): Vector[Nothing] = Vector.empty
    override def encodeArray(a: Unit): Vector[Nothing] = Vector.empty

  def apply[O <: Data, A](field: Field[O, A]): Fields[O, A] = new Fields[O, A]:
    override def toVector: Vector[Field[?, ?]] = Vector(field)
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[A] =
      val value = data.collectFirst { case (name, data) if name === field.name => data }
      field.decode(value.getOrElse(Data.Null)).leftMap(field.name /: _)
    override def decodeArray(data: Vector[Data]): Codec.Result[A] = data match
      case Vector(head) => field.decode(head).leftMap(field.name /: _)
      case Vector() =>
        Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 1), actual = Data.Number(0))).invalid
      case _ =>
        Violations
          .rootNec(Violation(Constraint.Collection.MaxItems(reference = 1), actual = Data.Number(data.length)))
          .invalid
    override def encodeRecord(a: A, nulls: Null): Vector[(String, O)] = field.encode(a, nulls).toVector
    override def encodeArray(a: A): Vector[O] = Vector(field.encode(a))
