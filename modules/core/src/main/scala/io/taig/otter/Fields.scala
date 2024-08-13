package io.taig.otter

import cats.syntax.all.*
import cats.data.Validated

sealed abstract class Fields[+O <: Data, A]:
  self =>

  def toVector: Vector[Field[?, ?]]

  final def imap[B](f: A => B)(g: B => A): Fields[O, B] = new Fields[O, B]:
    export self.toVector
    override def decodeTuple(data: Vector[Data]): Codec.Result[B] = self.decodeTuple(data).map(f)
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[B] = self.decodeRecord(data).map(f)
    override def encodeTuple(b: B): Vector[O] = self.encodeTuple(g(b))
    override def encodeRecord(b: B, nulls: Null): Vector[(String, O)] = self.encodeRecord(g(b), nulls)

  final def to[B](using convert: Convert[A, B]): Fields[O, B] = imap(convert.to)(convert.from)

  final def zip[P <: Data, B](fields: Fields[P, B]): Fields[O | P, (A, B)] = new Fields[O | P, (A, B)]:
    override def toVector: Vector[Field[?, ?]] = self.toVector ++ fields.toVector
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[((A, B))] =
      val (left, remainders) = data.filterKeys(self.toVector.map(_.name))
      val (right, _) = remainders.filterKeys(fields.toVector.map(_.name))
      (self.decodeRecord(left), fields.decodeRecord(right)).tupled
    override def decodeTuple(data: Vector[Data]): Codec.Result[(A, B)] =
      val (left, right) = data.splitAt(self.toVector.length)

      self.decodeTuple(left) match
        case Validated.Valid(a) => fields.decodeTuple(right).tupleLeft(a)
        case Validated.Invalid(violations) =>
          fields.decodeTuple(right).fold(violations.combine, _ => violations).invalid
    override def encodeRecord(ab: (A, B), nulls: Null): Vector[(String, O | P)] =
      self.encodeRecord(ab._1, nulls) ++ fields.encodeRecord(ab._2, nulls)
    override def encodeTuple(ab: (A, B)): Vector[O | P] = self.encodeTuple(ab._1) ++ fields.encodeTuple(ab._2)

  final def :*[P <: Data, B](field: Field[P, B])(using merge: Merge[A, B]): Fields[O | P, merge.Out] =
    zip(field.toFields).imap(merge.apply)(merge.unapply)

  final def *:[P <: Data, B](field: Field[P, B])(using merge: Merge[B, A]): Fields[P | O, merge.Out] =
    field.toFields.zip(this).imap(merge.apply)(merge.unapply)

  def decodeRecord(data: Vector[(String, Data)]): Codec.Result[A]

  def decodeTuple(data: Vector[Data]): Codec.Result[A]

  def encodeRecord(a: A, nulls: Null): Vector[(String, O)]

  def encodeTuple(a: A): Vector[O]

object Fields:
  val Empty: Fields[Nothing, Unit] = new Fields[Nothing, Unit]:
    override def toVector: Vector[Nothing] = Vector.empty
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[Unit] = ().valid
    override def decodeTuple(data: Vector[Data]): Codec.Result[Unit] = ().valid
    override def encodeRecord(a: Unit, nulls: Null): Vector[Nothing] = Vector.empty
    override def encodeTuple(a: Unit): Vector[Nothing] = Vector.empty

  def apply[O <: Data, A](field: => Field[O, A]): Fields[O, A] = new Fields[O, A]:
    override def toVector: Vector[Field[?, ?]] = Vector(field)
    override def decodeRecord(data: Vector[(String, Data)]): Codec.Result[A] =
      val value = data.collectFirst { case (name, data) if name === field.name => data }
      field.decode(value.getOrElse(Data.Null)).leftMap(field.name /: _)
    override def decodeTuple(data: Vector[Data]): Codec.Result[A] = data match
      case Vector(head) => field.decode(head).leftMap(field.name /: _)
      case Vector() =>
        Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 1), actual = Data.Number(0))).invalid
      case _ =>
        Violations
          .rootNec(Violation(Constraint.Collection.MaxItems(reference = 1), actual = Data.Number(data.length)))
          .invalid
    override def encodeRecord(a: A, nulls: Null): Vector[(String, O)] = field.encode(a, nulls).toVector
    override def encodeTuple(a: A): Vector[O] = Vector(field.encode(a))
