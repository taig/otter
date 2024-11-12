package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Nullable[+O <: Data, A] extends Codec[Data.Nullable[O], A]:
  self =>

  def default: A

  final def modifyDefault(f: A => A): Nullable[O, A] = new Nullable[O, A]:
    export self.{encode, metadata}
    override def default: A = f(self.default)
    override def decode(data: Data, default: A): Codec.Result[A] = self.decode(data, default)

  override def modifyMetadata(f: Metadata => Metadata): Nullable[O, A] = new Nullable[O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Nullable[O, B] = new Nullable[O, B]:
    export self.metadata
    override def default: B = f(self.default)
    override def decode(data: Data, default: B): Codec.Result[B] = self.decode(data, g(default)).map(f)
    override def encode(b: B): Data.Nullable[O] = self.encode(g(b))

  final override def decode(data: Data): Codec.Result[A] = decode(data, default)

  protected def decode(data: Data, default: A): Codec.Result[A]

  override def to[B](using convert: Convert[A, B]): Nullable[O, B] = imap(convert.to)(convert.from)

object Nullable:
  final private case class Apply[+O <: Data, A](codec: Codec[O, A]) extends Nullable[O, Option[A]]:
    override def metadata: Metadata = codec.metadata
    override def default: Option[A] = none
    override def decode(data: Data, default: Option[A]): Codec.Result[Option[A]] =
      if data.isNull then default.valid else codec.decode(data).map(_.some)
    override def encode(a: Option[A]): Data.Nullable[O] = a.fold(Data.Null)(codec.encode)

  def apply[O <: Data, A](codec: Codec[O, A]): Nullable[O, Option[A]] = Apply(codec)
