package io.taig.otter

import cats.syntax.all.*
import cats.Eval

sealed abstract class Branch[+S <: Data.Any, A]:
  def name: String
  def codec: Eval[Codec[S, ?]]
  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Branch[S, A]
  final def imap[B](f: A => B)(g: B => A): Branch[S, B] = Branch.Modify(self = this, f, g)

  def :+[T <: Data.Any, B](branch: Branch[T, B]): Union.Untagged[S | T, Either[A, B]] =
    toUnion :+ branch

  final def toUnion: Union.Untagged[S, A] = Union.Untagged.Root(branch = this, metadata = Metadata.Empty)

object Branch:
  final private[otter] case class Modify[S <: Data.Any, A, B](self: Branch[S, A], f: A => B, g: B => A)
      extends Branch[S, B]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S <: Data.Any, A](name: String, codec: Eval[Codec[S, A]], metadata: Metadata)
      extends Branch[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, A] = copy(metadata = f(metadata))

  extension [S <: Data.Any, A <: Matchable](self: Branch[S, A])
    inline def |[T <: Data.Any, B <: Matchable](branch: Branch[T, B]): Union.Untagged[S | T, A | B] =
      self.toUnion | branch
