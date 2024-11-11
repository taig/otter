package io.taig.otter

import io.taig.otter.Codec.Result

sealed abstract class Branch[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
    export self.{codec, metadata, name}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final def to[B](using convert: Convert[A, B]): Branch[O, B] = imap(convert.to)(convert.from)

//   final def :+[P <: Data, B](branch: Branch[P, B]): Branches[O | P, Either[A, B]] = toBranches :+ branch

//   final def +:[P <: Data, B](branch: Branch[P, B]): Branches[P | O, Either[B, A]] = branch +: toBranches

//   final def toBranches: Branches[O, A] = Branches(this)

  def decode(data: Data): Codec.Result[A]

  def encode(a: A): O

object Branch:
  final private case class Apply[O <: Data, A](name: String, codec: Codec[O, A]) extends Branch[O, A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[A] = codec.decode(data)
    override def encode(a: A): O = codec.encode(a)

  def apply[O <: Data, A](name: String, codec: Codec[O, A]): Branch[O, A] = Apply(name, codec)

  // extension [O <: Data, A <: Matchable](self: Branch[O, A])
  //   inline def |[P <: Data, B <: Matchable](branch: Branch[P, B]): Branches[O | P, A | B] =
  //     self.toBranches | branch

  given [O <: Data, A]: Metadata.Ops[Branch[O, A]] with
    extension (self: Branch[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = self.modifyMetadata(f)
