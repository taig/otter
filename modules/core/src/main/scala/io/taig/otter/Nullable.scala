package io.taig.otter

import cats.syntax.all.*

sealed abstract class Nullable[+O <: Data, A] extends Codec[Data.Nullable[O], A]:
  override def modifyMetadata(f: Metadata => Metadata): Nullable[O, A] = ???
  override def modifyDefault(f: Option[A] => Option[A]): Nullable[O, A] = ???
  override def imap[B](f: A => B)(g: B => A): Nullable[O, B] = ???
  override def to[B](using convert: Convert[A, B]): Nullable[O, B] = imap(convert.to)(convert.from)

object Nullable:
  final private case class Apply[+O <: Data, A](codec: Codec[O, A]) extends Nullable[O, Option[A]]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Option[A]] = codec.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      if data.isNull then none.valid else codec.decode(data).map(_.some)
    override def encode(a: Option[A]): Data.Nullable[O] = a.fold(Data.Null)(codec.encode)

  def apply[O <: Data, A](codec: Codec[O, A]): Nullable[O, Option[A]] = Apply(codec)
