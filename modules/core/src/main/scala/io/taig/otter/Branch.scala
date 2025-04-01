package io.taig.otter

import cats.syntax.all.*

sealed abstract class Branch[+S[_], +T[_], A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]
  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  def :+[S1[a] >: S[a], T1[a] >: T[a], B](branch: Branch[S1, T1, B]): Union.Untagged[S1, T1, Either[A, B]] =
    toUnion :+ branch

  final def toUnion: Union.Untagged[S, T, A] = Union.Untagged.Root(branch = this, metadata = Metadata.Empty)

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))

  extension [S[_], T[_], A <: Matchable](self: Branch[S, T, A])
    inline def |[B <: Matchable](branch: Branch[S, T, B]): Union.Untagged[S, T, A | B] =
      self.toUnion | branch
