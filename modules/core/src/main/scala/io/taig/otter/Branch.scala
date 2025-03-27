package io.taig.otter

import cats.syntax.all.*
import cats.Eval

sealed abstract class Branch[+F <: Format.Any, A]:
  def name: String
  def codec: Eval[Codec[?, ?]]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[F, A]
  def imap[B](f: A => B)(g: B => A): Branch[F, B]

  def :+[G <: Format.Any, B](branch: Branch[G, B]): Union[F | G, Either[A, B]] =
    toUnion :+ branch

  def toUnion: Union[F, A] = Union.Root(branch = this, metadata = Metadata.Empty)

object Branch:
  extension [F <: Format.Any, A <: Matchable](self: Branch[F, A])
    inline def |[G <: Format.Any, B <: Matchable](branch: Branch[G, B]): Union[F | G, A | B] =
      self.toUnion | branch

  final case class Tagged[+F <: Format.Any, A](
      name: String,
      codec: Eval[Codec[F, A]],
      discriminator: Discriminator,
      metadata: Metadata
  ) extends Branch[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Branch.Tagged[F, A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Branch.Tagged[F, B] = copy(codec = codec.map(_.imap(f)(g)))

  final private[otter] case class Root[+F <: Format.Any, A](name: String, codec: Eval[Codec[F, A]], metadata: Metadata)
      extends Branch[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[F, A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Branch[F, B] = copy(codec = codec.map(_.imap(f)(g)))
