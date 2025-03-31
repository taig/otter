package io.taig.otter

import cats.syntax.all.*

sealed abstract class Branch[+S[_], A]:
  def name: String
  def codec: Reference[S, ?]
  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Branch[S, A]
  final def imap[B](f: A => B)(g: B => A): Branch[S, B] = Branch.Modify(self = this, f, g)

  def :+[T[a] >: S[a], B](branch: Branch[T, B]): Union.Untagged[T, Either[A, B]] =
    toUnion :+ branch

  final def toUnion: Union.Untagged[S, A] = Union.Untagged.Root(branch = this, metadata = Metadata.Empty)

object Branch:
  final private[otter] case class Modify[S[_], A, B](self: Branch[S, A], f: A => B, g: B => A) extends Branch[S, B]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S[_], A](name: String, codec: Reference[S, A], metadata: Metadata)
      extends Branch[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, A] = copy(metadata = f(metadata))

  extension [S[_], A <: Matchable](self: Branch[S, A])
    inline def |[B <: Matchable](branch: Branch[S, B]): Union.Untagged[S, A | B] =
      self.toUnion | branch
