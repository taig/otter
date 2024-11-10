package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.Data.Required

sealed abstract class Record[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Codec[F, Data.Object[O], A]:
  self =>

  def fields: Chain[Field[?, ?]]

  final override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = new Record[F, O, A]:
    export self.{decode, default, encode, encodeSequence, fields, isNullable}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Record[F, O, A] = new Record[F, O, A]:
    export self.{encode, encodeSequence, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def isNullable: Boolean = default.nonEmpty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(Option[Vector[(String, Data)]], A)] =
      (data, default) match
        case (None, Some(default)) => (data, default).valid
        case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Record[F, O, B] = new Record[F, O, B]:
    export self.{fields, isNullable, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(Option[Vector[(String, Data)]], B)] =
      self.decode(data).map(_.map(f))
    override def encode(b: B): F[Data.Object[O]] = self.encode(g(b))
    override def encodeSequence(b: B): Data.Object[F[O]] = self.encodeSequence(g(b))

  final override def to[B](using convert: Convert[A, B]): Record[F, O, B] = imap(convert.to)(convert.from)

  override def nullable: Record[Data.Nullable, O, Option[A]] = new Record[Data.Nullable, O, Option[A]]:
    export self.{fields, metadata}
    override def isNullable: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(
        data: Option[Vector[(String, Data)]]
    ): Codec.Result[(Option[Vector[(String, Data)]], Option[A])] =
      data match
        case Some(values) => self.decode(values.some).map(_.map(_.some))
        case None         => (data, default.flatten).valid
    override def encode(a: Option[A]): Data.Nullable[Data.Object[O]] =
      a.map(self.encode).getOrElse(Data.Null)
    override def encodeSequence(a: Option[A]): Data.Object[Data.Nullable[O]] =
      a.fold(Data.Object(self.fields.toVector.map(_.name).tupleRight(Data.Null)))(self.encodeSequence(_))

  final def zip[G[+a] <: Data.Nullable[a], P <: Data, B](
      codec: Record[G, P, B]
  ): Record[Data.Required, F[O] | G[P], (A, B)] = new Record[Data.Required, F[O] | G[P], (A, B)]:
    override def fields: Chain[Field[?, ?]] = self.fields ++ codec.fields
    override def isNullable: Boolean = false
    override def default: Option[(A, B)] = none
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(Option[Vector[(String, Data)]], (A, B))] =
      self
        .decode(data, treatMissingFieldsAsOptional = self.isNullable)
        .andThen:
          case (data, a) => codec.decode(data, treatMissingFieldsAsOptional = codec.isNullable).map(_.tupleLeft(a))
    override def encode(ab: (A, B)): Data.Object[F[O] | G[P]] = encodeSequence(ab)
    override def encodeSequence(ab: (A, B)): Data.Object[F[O] | G[P]] =
      self.encodeSequence(ab._1) ++ codec.encodeSequence(ab._2)

  final def :*[P <: Data, B](field: Field[P, B])(using merge: Merge[A, B]): Record[Data.Required, F[O] | P, merge.Out] =
    zip(field.toRecord).imap(merge.apply)(merge.unapply)

  final def *:[P <: Data, B](field: Field[P, B])(using merge: Merge[B, A]): Record[Data.Required, P | F[O], merge.Out] =
    field.toRecord.zip(this).imap(merge.apply)(merge.unapply)

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Object(values) => decode(values.some).map { case (_, a) => a }
    case Data.Null           => decode(none).map { case (_, a) => a }
    case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid

  final def decode(
      data: Option[Vector[(String, Data)]],
      treatMissingFieldsAsOptional: Boolean
  ): Codec.Result[(Option[Vector[(String, Data)]], A)] = (data, treatMissingFieldsAsOptional) match
    case (Some(values), true) =>
      val keys = values.map { case (key, _) => key }.toSet
      val fieldsMissing = fields.map(_.name).forall(!keys.contains(_))
      if fieldsMissing then decode(none) else decode(data)
    case _ => decode(data)

  def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(Option[Vector[(String, Data)]], A)]

  override def encode(a: A): F[Data.Object[O]]

  protected def encodeSequence(a: A): Data.Object[F[O]]

object Record:
  final private case class Apply[O <: Data, A](field: Field[O, A]) extends Record[Data.Required, O, A]:
    override def fields: Chain[Field[?, ?]] = Chain.one(field)
    override def isNullable: Boolean = false
    override def default: Option[A] = none
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(Option[Vector[(String, Data)]], A)] = data
      .toValid(Violations.rootNec(Violation.tpe("object", actual = "null")))
      .andThen(field.decode(_))
      .map(_.leftMap(_.some))
    override def encode(a: A): Data.Object[O] = Data.Object(field.encode(a))
    override def encodeSequence(a: A): Data.Object[O] = encode(a)

  def apply[O <: Data, A](field: Field[O, A]): Record[Data.Required, O, A] = Apply(field)

  given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Record[F, O, *]] with
    override def imap[A, B](fa: Record[F, O, A])(f: A => B)(g: B => A): Record[F, O, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Record[F, O, A]] with
    extension (self: Record[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = self.modifyMetadata(f)
