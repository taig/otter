package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated
import io.taig.otter.Data.Optional
import io.taig.otter.Codec.Result
import cats.Id as Identity

sealed abstract class Product[
    +F[+a <: Data] <: Data.Optional[a],
    +G[+a <: Data] <: Data.Object[a] | Data.Array[a],
    +O <: Data,
    A
] extends Codec[F, G[O], A]:
  self =>

  def fields: Fields[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Product[F, G, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Product[F, G, O, A]
  override def imap[B](f: A => B)(g: B => A): Product[F, G, O, B]
  override def optional: Product[Data.Optional, G, O, Option[A]]

sealed abstract class Record[+F[+a <: Data] <: Data.Optional[a]: Data.Ops, +O <: Data, A]
    extends Product[F, Data.Object, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = new Record[F, O, A]:
    export self.{decode, default, encode, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Record[F, O, A] = new Record[F, O, A]:
    export self.{encode, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
      (data, default) match
        case (None, Some(default)) => (data, default).valid
        case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Record[F, O, B] = new Record[F, O, B]:
    export self.{fields, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], B)] =
      self.decode(data).map(_.map(f))
    override def encode(b: B): F[Data.Object[O]] = self.encode(g(b))

  override def optional: Record[Data.Optional, O, Option[A]] = new Record[Data.Optional, O, Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], Option[A])] =
      // TODO check if data is empty by verifying if fields exist
      data.fold(Validated.valid(default.flatten).tupleLeft(data))(_ => self.decode(data).map(_.map(_.some)))
    override def encode(a: Option[A]): Data.Optional[Data.Object[O]] = a.map(self.encode).getOrElse(Data.Null)

  def zip[G[+a <: Data] <: Data.Optional[a]: Data.Ops, P <: Data, B](
      codec: Record[G, P, B]
  ): Record[Identity, F[O] | G[P], (A, B)] = new Record[Identity, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def default: Option[(A, B)] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Chain[(String, Data)]]): Result[(Option[Chain[(String, Data)]], (A, B))] =
      self.decode(data) match
        case Validated.Valid((data, a))    => codec.decode(data).map(_.tupleLeft(a))
        case Validated.Invalid(violations) => codec.decode(data).fold(violations.combine, _ => violations).invalid
    override def encode(ab: (A, B)): Data.Object[F[O] | G[P]] =
      self.encode(ab._1).sequence ++ codec.encode(ab._2).sequence

  final override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Object(values) => decode(values.some)
      case Data.Null           => decode(none)
      case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)]

sealed abstract class Tuple[+F[+a <: Data] <: Data.Optional[a]: Data.Ops, +O <: Data, A]
    extends Product[F, Data.Array, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{decode, default, encode, fields}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{encode, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)] =
      (data, default) match
        case (None, Some(default)) => (data, default).valid
        case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Tuple[F, O, B] = ???

  final override def optional: Tuple[Data.Optional, O, Option[A]] = new Tuple[Data.Optional, O, Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], Option[A])] =
      data match
        case Some(values) if values.forall(_ === Data.Null) =>
          (data.map(_.drop(self.fields.toVector.length)), default.flatten).valid
        case Some(_) => self.decode(data).map(_.map(_.some))
        case None    => (data.map(_.drop(self.fields.toVector.length)), default.flatten).valid
    override def encode(a: Option[A]): Data.Optional[Data.Array[O]] = a.map(self.encode).getOrElse(Data.Null)

  final def zip[G[+a <: Data] <: Data.Optional[a]: Data.Ops, P <: Data, B](
      codec: Tuple[G, P, B]
  ): Tuple[Identity, F[O] | G[P], (A, B)] = new Tuple[Identity, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = None
    override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], (A, B))] =
      self.decode(data) match
        case Validated.Valid((data, a))    => codec.decode(data).map(_.tupleLeft(a))
        case Validated.Invalid(violations) => codec.decode(data).fold(violations.combine, _ => violations).invalid
    override def encode(ab: (A, B)): Data.Array[F[O] | G[P]] =
      self.encode(ab._1).sequence(self.fields.toVector.length) ++ codec
        .encode(ab._2)
        .sequence(codec.fields.toVector.length)

  override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Null          => decode(none)
      case Data.Array(values) => decode(values.some)
      case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)]

object Tuple:
  def apply[O <: Data, A](fields: Fields[O, A]): Tuple[Identity, O, A] =
    val _fields = fields

    new Tuple[Identity, O, A]:
      override def fields: Fields[O, A] = _fields
      override def default: Option[A] = None
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
        .andThen(fields.decodeArray(_).map(_.leftMap(_.some)))
      override def encode(a: A): Data.Array[O] = Data.Array(fields.encodeArray(a))
