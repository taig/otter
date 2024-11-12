package io.taig.otter

import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping

abstract class Enumeration[A] extends Codec[Data.Primitive, A]:
  self =>

  def codec: Codec[?, ?]

  override def modifyMetadata(f: Metadata => Metadata): Enumeration[A] = new Enumeration[A]:
    export self.{codec, decode, encode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Enumeration[B] = new Enumeration[B]:
    export self.{codec, metadata}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Primitive = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Enumeration[B] = imap(convert.to)(convert.from)

object Enumeration:
  final private case class Apply[A, B](codec: Codec[Data.Primitive, A], mapping: Mapping[B, A]) extends Enumeration[B]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[B] = codec
      .decode(data)
      .andThen: a =>
        mapping
          .unapply(a)
          .toValid(Violations.rootNec(Violation.oneOf(mapping.values.toList.map(encode), actual = data)))
    override def encode(b: B): Data.Primitive = codec.encode(mapping(b))

  def apply[A, B](
      codec: Codec[Data.Primitive, A],
      mapping: Mapping[B, A]
  ): Enumeration[B] = Apply(codec, mapping)

  given CodecInvariant[Enumeration] with
    override def imap[A, B](fa: Enumeration[A])(f: A => B)(g: B => A): Enumeration[B] = fa.imap(f)(g)

  given [A]: Metadata.Ops[Enumeration[A]] with
    extension (self: Enumeration[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Enumeration[A] = self.modifyMetadata(f)
