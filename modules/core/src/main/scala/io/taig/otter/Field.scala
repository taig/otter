package io.taig.otter

import cats.syntax.all.*

final case class Field[+O <: Data, A](name: String, codec: Codec[O, A], metadata: Metadata):
  self =>

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = copy(metadata = f(metadata))

  final def nulls: Null = metadata.get(Keys.nulls).getOrElse(Null.Default)
  final def nulls(value: Null): Field[O, A] = self(Keys.nulls, value)
  final def hideNulls: Field[O, A] = nulls(Null.Hide)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = copy(codec = codec.imap(f)(g))

  final def optional: Field[Data.Nullable[O], Option[A]] = copy(codec = codec.nullable)

  final def to[B](using convert: Convert[A, B]): Field[O, B] = imap(convert.to)(convert.from)

  final def :*[P <: Data, B](field: => Field[P, B])(using merge: Merge[A, B]): Record[O | P, merge.Out] =
    toRecord :* field

  final def *:[P <: Data, B](field: => Field[P, B])(using merge: Merge[B, A]): Record[P | O, merge.Out] =
    field *: toRecord

  final def toRecord: Record[O, A] = Record(this)

  def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
    val (remainders, data) = values.collectFirstWithRemainders { case (`name`, data) => data }
    (remainders, codec.decode(data.getOrElse(Data.Null)).leftMap(name /: _))

  def encode(a: A): Option[(String, O)] = codec.encode(a) match
    case Data.Null if nulls === Null.Hide => none
    case data                             => (name, data).some

object Field:
  def apply[O <: Data, A](name: String, codec: Codec[O, A]): Field[O, A] =
    Field(name, codec, metadata = Metadata.Empty)

  given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
    extension (self: Field[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
