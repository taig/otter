package io.taig.otter

import cats.syntax.all.*
import cats.~>

sealed abstract class Branch[+S[_], +T[_], A]:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]
  def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, A]
  def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Branch[U, T, A]
  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Branch[U, T, B] = copy(self = self.leftMapK(fK))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(value = value.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Branch[U, T, B] = copy(key = key.mapK(fK))
