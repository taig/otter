package io.taig.otter

import cats.syntax.all.*

sealed abstract class Field[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = new Field[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = new Field[O, B]:
    export self.{codec, metadata, name}
    override def decode(data: Vector[(String, Data)]): Codec.Result[(Vector[(String, Data)], B)] = ???
    override def encode(a: B): Vector[(String, O)] = ???

  final def optional: Field[O, Option[A]] = new Field[O, Option[A]]:
    export self.{codec, metadata, name}
    override def decode(data: Vector[(String, Data)]): Codec.Result[(Vector[(String, Data)], Option[A])] =
      if data.exists { case (name, _) => name == self.name }
      then self.decode(data).map(_.map(_.some))
      else (data, none).valid
    override def encode(a: Option[A]): Vector[(String, O)] = a match
      case Some(a) => self.encode(a)
      case None    => Vector.empty

  final def maybe(nulls: Null): Field[Data.Nullable[O], Option[A]] = new Field[Data.Nullable[O], Option[A]]:
    export self.{codec, metadata, name}
    override def decode(data: Vector[(String, Data)]): Codec.Result[(Vector[(String, Data)], Option[A])] =
      if data.exists { case (name, _) => name == self.name }
      then self.decode(data).map(_.map(_.some))
      else self.decode((name, Data.Null) +: data).map(_.map(_.some))
    override def encode(a: Option[A]): Vector[(String, Data.Nullable[O])] = a match
      case Some(a)                     => self.encode(a)
      case None if nulls === Null.Show => Vector((name, Data.Null))
      case None                        => Vector.empty

  final def maybe: Field[Data.Nullable[O], Option[A]] = maybe(Null.Default)

  final def to[B](using convert: Convert[A, B]): Field[O, B] = imap(convert.to)(convert.from)

  final def :*[P <: Data, B](field: => Field[P, B])(using merge: Merge[A, B]): Record[Data.Required, O | P, merge.Out] =
    toRecord :* field

  final def *:[P <: Data, B](field: => Field[P, B])(using merge: Merge[B, A]): Record[Data.Required, P | O, merge.Out] =
    field *: toRecord

  final def toRecord: Record[Data.Required, O, A] = Record(this)

  def decode(data: Vector[(String, Data)]): Codec.Result[(Vector[(String, Data)], A)]

  def encode(a: A): Vector[(String, O)]

object Field:
  final private case class Apply[F[+a] <: Data.Nullable[a], O <: Data, A](name: String, codec: Codec[F, O, A])
      extends Field[F[O], A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Vector[(String, Data)]): Codec.Result[(Vector[(String, Data)], A)] =
      data.collectFirstWithRemainders { case (`name`, data) => data } match
        case Some((data, remainders)) => codec.decode(data).tupleLeft(remainders)
        case None =>
          Violations
            .namespaceNec(XPath.Root / name, Violation(Constraint.Type("value"), actual = Data.String("null")))
            .invalid
    override def encode(a: A): Vector[(String, F[O])] = Vector((name, codec.encode(a)))

  def apply[F[+a] <: Data.Nullable[a], O <: Data, A](name: String, codec: Codec[F, O, A]): Field[F[O], A] =
    Apply(name, codec)

  given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
    extension (self: Field[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
