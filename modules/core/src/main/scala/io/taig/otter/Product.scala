package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

sealed abstract class Product[
    +F[+a] <: Data.Nullable[a],
    +G[+a <: Data] <: Data.Object[a] | Data.Array[a],
    +O <: Data,
    A
] extends Codec[F, G[O], A]:
  self =>

  def fields: Fields[?, ?]

  override def modifyMetadata(f: Metadata => Metadata): Product[F, G, O, A]

  override def modifyDefault(f: Option[A] => Option[A]): Product[F, G, O, A]

  override def imap[B](f: A => B)(g: B => A): Product[F, G, O, B]

  override def to[B](using convert: Convert[A, B]): Product[F, G, O, B]

  override def nullable: Product[Data.Nullable, G, O, Option[A]]

object Product:
  given [F[+a] <: Data.Nullable[a], G[+a <: Data] <: Data.Object[a] | Data.Array[a], O <: Data]
      : CodecInvariant[Product[F, G, O, *]] with
    override def imap[A, B](fa: Product[F, G, O, A])(f: A => B)(g: B => A): Product[F, G, O, B] =
      fa.imap(f)(g)

  given [F[+a] <: Data.Nullable[a], G[+a <: Data] <: Data.Object[a] | Data.Array[a], O <: Data, A]
      : Metadata.Ops[Product[F, G, O, A]] with
    extension (self: Product[F, G, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Product[F, G, O, A] = self.modifyMetadata(f)

sealed abstract class Record[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Product[F, Data.Object, O, A]:
  self =>

  final def nulls: Attribute[Record[F, O, A], Null] = Attribute(this, Keys.nulls, Null.Default)

  final override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = new Record[F, O, A]:
    export self.{decode, default, encode, encodeSequence, fields, isNullable}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Record[F, O, A] = new Record[F, O, A]:
    export self.{encode, encodeSequence, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def isNullable: Boolean = default.nonEmpty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Record[F, O, B] = new Record[F, O, B]:
    export self.{fields, isNullable, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B, nulls: Null): F[Data.Object[O]] = self.encode(g(b), nulls)
    override def encodeSequence(b: B, nulls: Null): Data.Object[F[O]] = self.encodeSequence(g(b), nulls)

  final override def to[B](using convert: Convert[A, B]): Record[F, O, B] = imap(convert.to)(convert.from)

  override def nullable: Record[Data.Nullable, O, Option[A]] = new Record[Data.Nullable, O, Option[A]]:
    export self.{fields, metadata}
    override def isNullable: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[Option[A]] = data match
      case Some(values) if values.forall { case (_, data) => data === Data.Null } => default.flatten.valid
      case Some(_)                                                                => self.decode(data).map(_.some)
      case None                                                                   => default.flatten.valid
    override def encode(a: Option[A], nulls: Null): Data.Nullable[Data.Object[O]] =
      a.map(self.encode(_, nulls)).getOrElse(Data.Null)
    override def encodeSequence(a: Option[A], nulls: Null): Data.Object[Data.Nullable[O]] =
      a.fold(Data.Object(self.fields.toVector.map(_.name).tupleRight(Data.Null)))(self.encodeSequence(_, nulls))

  final def zip[G[+a] <: Data.Nullable[a], P <: Data, B](
      codec: Record[G, P, B]
  ): Record[Data.Required, F[O] | G[P], (A, B)] = new Record[Data.Required, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def isNullable: Boolean = false
    override def default: Option[(A, B)] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[(A, B)] =
      val split = data.map: values =>
        val (left, remainders) = values.filterKeys(self.fields.toVector.map(_.name))
        val (right, _) = remainders.filterKeys(codec.fields.toVector.map(_.name))
        (left, right)
      (self.decode(split.map(_._1)), codec.decode(split.map(_._2))).tupled
    override def encode(ab: (A, B), nulls: Null): Data.Object[F[O] | G[P]] = encodeSequence(ab, nulls)
    override def encodeSequence(ab: (A, B), nulls: Null): Data.Object[F[O] | G[P]] =
      self.encodeSequence(ab._1, nulls) ++ codec.encodeSequence(ab._2, nulls)

  final def :*[G[+a] <: Data.Nullable[a], P <: Data, B](field: Field[G[P], B])(using
      merge: Merge[A, B]
  ): Record[Data.Required, F[O] | G[P], merge.Out] =
    zip(Record(field.toFields)).imap(merge.apply)(merge.unapply)

  final def *:[G[+a] <: Data.Nullable[a], P <: Data, B](field: Field[G[P], B])(using
      merge: Merge[B, A]
  ): Record[Data.Required, G[P] | F[O], merge.Out] =
    Record(field.toFields).zip(this).imap(merge.apply)(merge.unapply)

  final override def decode(data: Data): Codec.Result[A] = data match
    case Data.Object(values) => decode(values.some)
    case Data.Null           => decode(none)
    case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A]

  final override def encode(a: A): F[Data.Object[O]] = encode(a, nulls.value)

  def encode(a: A, nulls: Null): F[Data.Object[O]]

  protected def encodeSequence(a: A, nulls: Null): Data.Object[F[O]]

object Record:
  def apply[O <: Data, A](of: => Fields[O, A]): Record[Data.Required, O, A] = new Record[Data.Required, O, A]:
    override def fields: Fields[?, ?] = of
    override def isNullable: Boolean = false
    override def default: Option[A] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[A] = data
      .toValid(Violations.rootNec(Violation.tpe("object", actual = "null")))
      .andThen(of.decodeRecord)
    override def encode(a: A, nulls: Null): Data.Object[O] = Data.Object(of.encodeRecord(a, nulls))
    override def encodeSequence(a: A, nulls: Null): Data.Object[O] = encode(a, nulls)

  given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Record[F, O, *]] with
    override def imap[A, B](fa: Record[F, O, A])(f: A => B)(g: B => A): Record[F, O, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Record[F, O, A]] with
    extension (self: Record[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = self.modifyMetadata(f)

sealed abstract class Tuple[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Product[F, Data.Array, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{decode, default, encode, encodeSequence, fields, isNullable}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple[F, O, A] = new Tuple[F, O, A]:
    export self.{encode, encodeSequence, fields, metadata}
    override def default: Option[A] = f(self.default)
    override def isNullable: Boolean = default.nonEmpty
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Tuple[F, O, B] = new Tuple[F, O, B]:
    export self.{fields, isNullable, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Option[Vector[Data]]): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[Data.Array[O]] = self.encode(g(b))
    override def encodeSequence(b: B): Data.Array[F[O]] = self.encodeSequence(g(b))

  final override def to[B](using convert: Convert[A, B]): Tuple[F, O, B] = imap(convert.to)(convert.from)

  final override def nullable: Tuple[Data.Nullable, O, Option[A]] = new Tuple[Data.Nullable, O, Option[A]]:
    export self.{fields, metadata}
    override def isNullable: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[Data]]): Codec.Result[Option[A]] = data match
      case Some(values) if values.forall(_ === Data.Null) => default.flatten.valid
      case Some(_)                                        => self.decode(data).map(_.some)
      case None                                           => default.flatten.valid
    override def encode(a: Option[A]): Data.Nullable[Data.Array[O]] = a.map(self.encode).getOrElse(Data.Null)
    override def encodeSequence(a: Option[A]): Data.Array[Data.Nullable[O]] =
      a.fold(Data.Array.fill(fields.toVector.length)(Data.Null))(self.encodeSequence)

  final def zip[G[+a] <: Data.Nullable[a], P <: Data, B](
      codec: Tuple[G, P, B]
  ): Tuple[Data.Required, F[O] | G[P], (A, B)] = new Tuple[Data.Required, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.zip(codec.fields)
    override def isNullable: Boolean = false
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = None
    override def decode(data: Option[Vector[Data]]): Codec.Result[(A, B)] =
      val split = data.map(_.splitAt(self.fields.toVector.length))
      (self.decode(split.map(_._1)), codec.decode(split.map(_._2))).tupled
    override def encode(ab: (A, B)): Data.Array[F[O] | G[P]] = encodeSequence(ab)
    override def encodeSequence(ab: (A, B)): Data.Array[F[O] | G[P]] =
      self.encodeSequence(ab._1) ++ codec.encodeSequence(ab._2)

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

  protected def encodeSequence(a: A): Data.Array[F[O]]

object Tuple:
  def apply[O <: Data, A](of: => Fields[O, A]): Tuple[Data.Required, O, A] = new Tuple[Data.Required, O, A]:
    override def fields: Fields[?, ?] = of
    override def isNullable: Boolean = false
    override def default: Option[A] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = data
      .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
      .andThen(of.decodeTuple)
    override def encode(a: A): Data.Array[O] = Data.Array(of.encodeTuple(a))
    override def encodeSequence(a: A): Data.Array[O] = encode(a)

  given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Tuple[F, O, *]] with
    override def imap[A, B](fa: Tuple[F, O, A])(f: A => B)(g: B => A): Tuple[F, O, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Tuple[F, O, A]] with
    extension (self: Tuple[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Tuple[F, O, A] = self.modifyMetadata(f)
