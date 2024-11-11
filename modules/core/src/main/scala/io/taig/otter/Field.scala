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
    override def decode(data: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[B]) =
      self.decode(data).map(_.map(f))
    override def encode(b: B): Vector[(String, O)] = self.encode(g(b))

  // TODO default (?)
  final def optional: Field[O, Option[A]] = new Field[O, Option[A]]:
    export self.{codec, metadata, name}
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[Option[A]]) =
      if values.exists { case (name, _) => name == self.name }
      then self.decode(values).map(_.map(_.some))
      else (values, none.valid)
    override def encode(a: Option[A]): Vector[(String, O)] = a.fold(Vector.empty)(self.encode)

  final def nullable: Field[O, Option[A]] = ???

  // TODO default (?)
  final def maybe(nulls: Null): Field[Data.Nullable[O], Option[A]] = new Field[Data.Nullable[O], Option[A]]:
    export self.{codec, metadata, name}
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[Option[A]]) =
      values.collectFirstWithRemainders { case (name, data) if name == self.name => data } match
        case (values, Some(Data.Null)) => (values, none.valid)
        case (_, Some(data))           => self.decode(values).map(_.map(_.some))
        case (values, None)            => (values, none.valid)
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

  def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A])

  def encode(a: A): Vector[(String, O)]

object Field:
  extension [O <: Data, A](self: Field[Data.Nullable[O], A]) def test: Field[Data.Nullable[O], A] = ???

  final private case class Apply[F[+a] <: Data.Nullable[a], O <: Data, A](name: String, codec: Codec[F, O, A])
      extends Field[F[O], A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
      values.collectFirstWithRemainders { case (`name`, data) => data } match
        case (values, Some(data)) => (values, codec.decode(data).leftMap(name /: _))
        case (values, None) =>
          (
            values,
            Violations
              .namespaceNec(XPath.Root / name, Violation(Constraint.Type("value"), actual = Data.String("null")))
              .invalid
          )
    override def encode(a: A): Vector[(String, F[O])] = Vector((name, codec.encode(a)))

  def apply[F[+a] <: Data.Nullable[a], O <: Data, A](name: String, codec: Codec[F, O, A]): Field[F[O], A] =
    Apply(name, codec)

  given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
    extension (self: Field[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
