package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Field[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = new Field[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = new Field[O, B]:
    export self.{codec, metadata, name}
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[B]) =
      self.decode(values).map(_.map(f))
    override def encode(b: B): Option[(String, O)] = self.encode(g(b))

  final def to[B](using convert: Convert[A, B]): Field[O, B] = imap(convert.to)(convert.from)

  final def :*[P <: Data, B](field: => Field[P, B])(using merge: Merge[A, B]): Record[O | P, merge.Out] =
    toRecord :* field

  final def *:[P <: Data, B](field: => Field[P, B])(using merge: Merge[B, A]): Record[P | O, merge.Out] =
    field *: toRecord

  final def toRecord: Record[O, A] = Record(this)

  def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A])

  def encode(a: A): Option[(String, O)]

object Field:
  final private[otter] case class Nullable[O <: Data, A](name: String, codec: Codec[O, A]) extends Field[O, A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.decode(value.getOrElse(Data.Null)).leftMap(name /: _))
    override def encode(a: A): Option[(String, O)] = (name, codec.encode(a)).some

  final private[otter] case class Optional[O <: Data, A](name: String, codec: Codec[O, A])
      extends Field[Data.Value.Of[O], A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Vector[(String, Data)]): (Vector[(String, Data)], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.decode(value.getOrElse(Data.Null)).leftMap(name /: _))
    override def encode(a: A): Option[(String, Data.Value.Of[O])] =
      Data.Value.of(codec.encode(a)).tupleLeft(name)

  given [O <: Data, A]: Metadata.Ops[Field[O, A]] with
    extension (self: Field[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Field[O, A] = self.modifyMetadata(f)
