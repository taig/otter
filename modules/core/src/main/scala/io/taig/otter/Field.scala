package io.taig.otter

import cats.syntax.all.*
import cats.Eval

sealed abstract class Field[+O <: Data, A]:
  def name: String

  def codec: Eval[Codec[?, ?]]

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Field[O, A]

  def imap[B](f: A => B)(g: B => A): Field[O, B]

  def to[B](using convert: Convert[A, B]): Field[O, B]

  final def :*[P <: Data, B](field: Field[P, B])(using merge: Merge[A, B]): Record[O | P, merge.Out] =
    toRecord :* field

  final def *:[P <: Data, B](field: Field[P, B])(using merge: Merge[B, A]): Record[P | O, merge.Out] =
    field *: toRecord

  final def toRecord: Record[O, A] = Record.Apply(this)

  def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A])

  def encode(a: A): Option[(String, O)]

object Field:
  final case class Required[O <: Data.Value, A](name: String, codec: Eval[Codec[O, A]], metadata: Metadata)
      extends Field[O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Field.Required[O, B] = copy(codec = codec.map(_.imap(f)(g)))
    override def to[B](using convert: Convert[A, B]): Field.Required[O, B] = imap(convert.to)(convert.from)
    def nullable: Field[Data.Nullable[O], Option[A]] = Field.Nullable(name, codec = codec.map(_.nullable), metadata)
    def optional: Field[O, Option[A]] = Field.Optional(name, codec = codec.map(_.nullable), metadata)
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.value.decode(value.getOrElse(Data.Null)).leftMap(name /: _))
    override def encode(a: A): Option[(String, O)] = (name, codec.value.encode(a)).some

  final private[otter] case class Nullable[O <: Data, A](name: String, codec: Eval[Codec[O, A]], metadata: Metadata)
      extends Field[O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Field[O, B] = copy(codec = codec.map(_.imap(f)(g)))
    override def to[B](using convert: Convert[A, B]): Field[O, B] = imap(convert.to)(convert.from)
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.value.decode(value.getOrElse(Data.Null)).leftMap(name /: _))
    override def encode(a: A): Option[(String, O)] = (name, codec.value.encode(a)).some

  final private[otter] case class Optional[O <: Data.Value, A](
      name: String,
      codec: Eval[Codec[Data.Nullable[O], A]],
      metadata: Metadata
  ) extends Field[O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Field[O, B] = copy(codec = codec.map(_.imap(f)(g)))
    override def to[B](using convert: Convert[A, B]): Field[O, B] = imap(convert.to)(convert.from)
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.value.decode(value.getOrElse(Data.Null)).leftMap(name /: _))
    override def encode(a: A): Option[(String, O)] = codec.value.encode(a).asValue.tupleLeft(name)

  given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
    extension (self: Field[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
