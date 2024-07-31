package io.taig.otter

import io.taig.otter.Codec.Result

sealed abstract class Branch[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
    export self.{codec, decode, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
    export self.{codec, metadata, name}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final def :+[P <: Data, B](branch: Branch[P, B]): Branches[O | P, Either[A, B]] = toBranches :+ branch

  final def +:[P <: Data, B](branch: Branch[P, B]): Branches[P | O, Either[B, A]] = branch +: toBranches

  final def toBranches: Branches[O, A] = Branches(this)

  def decode(data: Data): Codec.Result[A]

  def encode(a: A): O

object Branch:
  def apply[F[+a <: Data] <: Data.Optional[a], O <: Data, A](
      name: String,
      codec: Codec[F, O, A]
  ): Branch[F[O], A] =
    val _name = name
    val _codec = codec

    new Branch[F[O], A]:
      override def name: String = _name
      override def codec: Codec[F, O, A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(data: Data): Codec.Result[A] = codec.decode(data)
      override def encode(a: A): F[O] = codec.encode(a)
