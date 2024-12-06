package io.taig.otter

import cats.Eval
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping

abstract class Enumeration[+O <: Data.Primitive, A] extends Codec[O, A]:
  self =>

  def codec: Eval[Codec[?, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Enumeration[O, A] = new Enumeration[O, A]:
    export self.{codec, decode, encode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Enumeration[O, B] = new Enumeration[O, B]:
    export self.{codec, metadata}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Enumeration[O, B] = imap(convert.to)(convert.from)

object Enumeration:
  final private[otter] case class Apply[O <: Data.Primitive, A, B](codec: Eval[Codec[O, A]], mapping: Mapping[B, A])
      extends Enumeration[O, B]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[B] = codec.value
      .decode(data)
      .andThen: a =>
        mapping
          .unapply(a)
          .toValid(Violations.rootNec(Violation.oneOf(mapping.values.toList.map(encode), actual = data)))
    override def encode(b: B): O = codec.value.encode(mapping(b))

  given [O <: Data.Primitive]: CodecInvariant[Enumeration[O, *]] with
    override def imap[A, B](fa: Enumeration[O, A])(f: A => B)(g: B => A): Enumeration[O, B] = fa.imap(f)(g)

  given [O <: Data.Primitive, A]: Metadata.Ops[Enumeration[O, A]] with
    extension (self: Enumeration[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Enumeration[O, A] = self.modifyMetadata(f)
