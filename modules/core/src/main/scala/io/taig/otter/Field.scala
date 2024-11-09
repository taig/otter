package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Field[+F[+a] <: Data.Nullable[a], +O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[F, O, A] = new Field[F, O, A]:
    export self.{codec, decodeRecordValue, encodeRecordValue, name, decodeTupleValue}
    override def metadata: Metadata = f(self.metadata)
    override def encodeTupleValue(a: A): F[O] = ???

  final def imap[B](f: A => B)(g: B => A): Field[F, O, B] = new Field[F, O, B]:
    export self.{codec, metadata, name}
    override def decodeRecordValue(data: Option[Data]): Codec.Result[B] =
      self.decodeRecordValue(data).map(f)
    override def decodeTupleValue(data: Data): Codec.Result[B] =
      self.decodeTupleValue(data).map(f)
    override def encodeRecordValue(b: B): Option[F[O]] = self.encodeRecordValue(g(b))
    override def encodeTupleValue(b: B): F[O] = self.encodeTupleValue(g(b))

  final def optional: Field[Data.Nullable, O, Option[A]] = new Field[Data.Nullable, O, Option[A]]:
    export self.{codec, metadata, name}
    override def decodeRecordValue(data: Option[Data]): Codec.Result[Option[A]] = data match
        case Some(data) => self.decodeRecordValue(data.some).map(_.some)
        case None       => none.valid
    override def decodeTupleValue(data: Data): Codec.Result[Option[A]] = ???
    override def encodeRecordValue(a: Option[A]): Option[F[O]] =
      a.flatMap(self.encodeRecordValue)
    override def encodeTupleValue(a: Option[A]): Data.Nullable[O] =
      a.flatMap(self.encodeRecordValue).getOrElse(Data.Null)

  final def maybe(nulls: Null): Field[Data.Nullable, O, Option[A]] = new Field[Data.Nullable, O, Option[A]]:
    export self.{codec, metadata, name}
    override def decodeRecordValue(data: Option[Data]): Codec.Result[Option[A]] =
      data.traverse(data => self.decodeRecordValue(data.some))
    override def decodeTupleValue(data: Data): Codec.Result[Option[A]] = 
      self.decodeTupleValue(data).map(_.some)
    override def encodeRecordValue(a: Option[A]): Option[Data.Nullable[O]] =
      (a.flatMap(self.encodeRecordValue), nulls) match
        case (None, Null.Show) => Data.Null.some
        case (a, _) => a
    override def encodeTupleValue(a: Option[A]): Data.Nullable[O] =
      a.map(self.encodeTupleValue).getOrElse(Data.Null)

  final def maybe: Field[Data.Nullable, O, Option[A]] = maybe(Null.Default)

  // final def to[B](using convert: Convert[A, B]): Field[O, B] = imap(convert.to)(convert.from)

  // final def :*[P <: Data, B](field: => Field[P, B])(using merge: Merge[A, B]): Fields[O | P, merge.Out] =
  //   toFields :* field

  // final def *:[P <: Data, B](field: => Field[P, B])(using merge: Merge[B, A]): Fields[P | O, merge.Out] =
  //   field *: toFields

  final def toFields: Fields[O, A] = Fields(this)

  def decodeRecordValue(data: Option[Data]): Codec.Result[A]

  def decodeTupleValue(data: Data): Codec.Result[A]

  final def encordRecord(a: A): Option[(String, F[O])] = encodeRecordValue(a).tupleLeft(name)

  def encodeRecordValue(a: A): Option[F[O]]

  final def encodeRecord(a: A): (String, F[O]) = (name, encodeTupleValue(a))

  def encodeTupleValue(a: A): F[O]

object Field:
  final private case class Apply[F[+a] <: Data.Nullable[a], O <: Data, A](name: String, codec: Codec[F, O, A])
      extends Field[Data.Required, F[O], A]:
    override def metadata: Metadata = Metadata.Empty
    // override def decode(data: Data): Codec.Result[A] = codec.decode(data)
    override def decodeRecordValue(data: Option[Data]): Codec.Result[A] = data match
      case Some(data) => codec.decode(data)
      case None =>
        Violations
          .namespaceNec(XPath.Root / name, Violation(Constraint.Type("value"), actual = Data.String("null")))
          .invalid
    override def decodeTupleValue(data: Data): Codec.Result[A] = codec.decode(data).leftMap(name /: _)
    override def encodeRecordValue(a: A): Option[F[O]] = encodeTupleValue(a).some
    override def encodeTupleValue(a: A): F[O] = codec.encode(a)

  def apply[F[+a] <: Data.Nullable[a], O <: Data, A](
      name: String,
      codec: => Codec[F, O, A]
  ): Field[Data.Required, F[O], A] = Apply(name, codec)

  // given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
  //   extension (self: Field[O, A])
  //     override def metadata: Metadata = self.metadata
  //     override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
