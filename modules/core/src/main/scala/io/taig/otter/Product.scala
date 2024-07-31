package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated
import io.taig.otter.Keys.*
import cats.Id as Identity

sealed abstract class Product[+F[+a <: Data] <: Data.Optional[a], +O <: Data.Object[?] | Data.Array[?], A]
    extends Codec[F, O, A]:
  self =>

  def fields: Fields[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Product[F, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Product[F, O, A]
  override def imap[B](f: A => B)(g: B => A): Product[F, O, B]
  override def optional: Product[Data.Optional, O, Option[A]]

sealed abstract class Record[+F[+a <: Data] <: Data.Optional[a]: Data.Ops, +O <: Data, A]
    extends Product[F, Data.Object[O], A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = new Record[F, O, A]:
    export self.{decode, default, encode, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Record[F, O, A] = new Record[F, O, A]:
    export self.{encode, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Record[F, O, B] = new Record[F, O, B]:
    export self.{fields, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B, nulls: Null): F[Data.Object[O]] = self.encode(g(b), nulls)

  override def optional: Record[Data.Optional, O, Option[A]] = new Record[Data.Optional, O, Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[Option[A]] = data match
      case Some(values) if values.forall { case (_, data) => data === Data.Null } => default.flatten.valid
      case Some(_)                                                                => self.decode(data).map(_.some)
      case None                                                                   => default.flatten.valid
    override def encode(a: Option[A], nulls: Null): Data.Optional[Data.Object[O]] =
      a.map(self.encode(_, nulls)).getOrElse(Data.Null)

  def zip[G[+a <: Data] <: Data.Optional[a]: Data.Ops, P <: Data, B](
      codec: Record[G, P, B]
  ): Record[Identity, F[O] | G[P], (A, B)] = new Record[Identity, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def default: Option[(A, B)] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(A, B)] =
      val split = data.map: values =>
        val (left, remainders) = values.filterKeys(self.fields.toVector.map(_.name))
        val (right, _) = remainders.filterKeys(codec.fields.toVector.map(_.name))
        (left, right)
      (self.decode(split.map(_._1)), codec.decode(split.map(_._2))).tupled
    override def encode(ab: (A, B), nulls: Null): Data.Object[F[O] | G[P]] =
      self.encode(ab._1).sequence(self.fields.toVector.map(_.name)) ++
        codec.encode(ab._2).sequence(codec.fields.toVector.map(_.name))

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Object(values) => decode(values.some)
    case Data.Null           => decode(none)
    case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A]

  override def encode(a: A): F[Data.Object[O]] =
    encode(a, metadata(nulls).getOrElse(Null.Default))

  def encode(a: A, nulls: Null): F[Data.Object[O]]

object Record:
  def apply[O <: Data, A](fields: Fields[O, A]): Record[Identity, O, A] =
    val _fields = fields

    new Record[Identity, O, A]:
      override def fields: Fields[O, A] = _fields
      override def default: Option[A] = None
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))))
        .andThen(fields.decodeRecord)
      override def encode(a: A, nulls: Null): Data.Object[O] =
        Data.Object(fields.encodeRecord(a, nulls))

sealed abstract class Tuple[+F[+a <: Data] <: Data.Optional[a]: Data.Ops, +O <: Data, A]
    extends Product[F, Data.Array[O], A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{decode, default, encode, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{encode, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Tuple[F, O, B] = new Tuple[F, O, B]:
    export self.{fields, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[Data]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Array[O]] = self.encode(g(b))

  final override def optional: Tuple[Data.Optional, O, Option[A]] = new Tuple[Data.Optional, O, Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[Data]]): Codec.Result[Option[A]] = data match
      case Some(values) if values.forall(_ === Data.Null) => default.flatten.valid
      case Some(_)                                        => self.decode(data).map(_.some)
      case None                                           => default.flatten.valid
    override def encode(a: Option[A]): Data.Optional[Data.Array[O]] = a.map(self.encode).getOrElse(Data.Null)

  final def zip[G[+a <: Data] <: Data.Optional[a]: Data.Ops, P <: Data, B](
      codec: Tuple[G, P, B]
  ): Tuple[Identity, F[O] | G[P], (A, B)] = new Tuple[Identity, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = None
    override def decode(data: Option[Vector[Data]]): Codec.Result[(A, B)] =
      val split = data.map(_.splitAt(self.fields.toVector.length))

      self.decode(split.map(_._1)) match
        case Validated.Valid(a) => codec.decode(split.map(_._2)).tupleLeft(a)
        case Validated.Invalid(violations) =>
          codec.decode(split.map(_._2)).fold(violations.combine, _ => violations).invalid
    override def encode(ab: (A, B)): Data.Array[F[O] | G[P]] =
      self.encode(ab._1).sequence(self.fields.toVector.length) ++
        codec.encode(ab._2).sequence(codec.fields.toVector.length)

  override def decode(data: Data): Codec.Result[A] = data match
    case Data.Null => decode(none)
    case Data.Array(values) =>
      val length = self.fields.toVector.length

      if values.length < length
      then
        Violations
          .rootNec(Violation(Constraint.Collection.MinItems(reference = length), actual = Data.Number(values.length)))
          .invalid
      else if values.length > length
      then
        Violations
          .rootNec(Violation(Constraint.Collection.MaxItems(reference = length), actual = Data.Number(values.length)))
          .invalid
      else decode(values.some)
    case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[Data]]): Codec.Result[A]

object Tuple:
  def apply[O <: Data, A](fields: Fields[O, A]): Tuple[Identity, O, A] =
    val _fields = fields

    new Tuple[Identity, O, A]:
      override def fields: Fields[O, A] = _fields
      override def default: Option[A] = None
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Option[Vector[Data]]): Codec.Result[A] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
        .andThen(fields.decodeArray)
      override def encode(a: A): Data.Array[O] = Data.Array(fields.encodeArray(a))
