package io.taig.otter

import cats.syntax.all.*

sealed abstract class Nullable[+O <: Data.Value, A] extends Codec[Data.Nullable[O], A]:
  self =>

  def default: A

  override def modifyMetadata(f: Metadata => Metadata): Nullable[O, A] = new Nullable[O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Nullable[O, B] = new Nullable[O, B]:
    export self.metadata
    override def default: B = f(self.default)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Nullable[O] = self.encode(g(b))

  override def to[B](using convert: Convert[A, B]): Nullable[O, B] = imap(convert.to)(convert.from)

object Nullable:
  final private case class Apply[O <: Data.Value, A](codec: Codec[O, A]) extends Nullable[O, Option[A]]:
    override def metadata: Metadata = codec.metadata
    override def default: Option[A] = none
    override def decode(data: Data): Codec.Result[Option[A]] =
      if data.isNull then none.valid else codec.decode(data).map(_.some)
    override def encode(a: Option[A]): Data.Nullable[O] = a.map(codec.encode).getOrElse(Data.Null)

  final private case class Default[O <: Data.Value, A](codec: Codec[O, A], default: A) extends Nullable[O, A]:
    override def metadata: Metadata = codec.metadata
    override def decode(data: Data): Codec.Result[A] = if data.isNull then default.valid else codec.decode(data)
    override def encode(a: A): Data.Nullable[O] = codec.encode(a)

  def apply[O <: Data.Value, A](codec: Codec[O, A]): Nullable[O, Option[A]] = Apply(codec)

  def apply[O <: Data.Value, A](codec: Codec[O, A], default: A): Nullable[O, A] = Default(codec, default)
