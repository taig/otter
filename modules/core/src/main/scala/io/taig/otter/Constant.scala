package io.taig.otter

import cats.Eq
import cats.syntax.all.*
import cats.~>

sealed abstract class Constant[+S[_], A]:
  def metadata: Metadata
  def codec: Reference.Constant[S, ?]
  def matches(a: A): Boolean
  def modifyMetadata(f: Metadata => Metadata): Constant[S, A]
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Constant[T, A]
  final def imap[B](f: A => B)(g: B => A): Constant[S, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[S[_], A, B](self: Constant[S, A], f: A => B, g: B => A) extends Constant[S, B]:
    export self.{codec, metadata}
    override def matches(b: B): Boolean = self.matches(g(b))
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Constant[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A: Eq](
      codec: Reference.Constant[S, A],
      metadata: Metadata
  ) extends Constant[S, A]:
    override def matches(a: A): Boolean = codec.value === a
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Constant[T, A] = copy(codec = codec.mapK(fK))
