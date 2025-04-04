package io.taig.otter

import cats.data.Chain
import cats.~>

sealed abstract class Record[+S[_], A] extends Codec[S, A]:
  def isOptional: Boolean
  def fields: Chain[(String, Reference[S, ?])]
  override def modifyMetadata(f: Metadata => Metadata): Record[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Record[T, A]
  final override def imap[B](f: A => B)(g: B => A): Record[S, B] = Record.Modify(self = this, f, g)
  final def optional: Record[S, Option[A]] = Record.Optional(self = this)
  final def zip[S1[a] >: S[a], B](codec: Record[S1, B]): Record[S1, (A, B)] =
    Record.Zip(left = this, right = codec, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def isOptional: Boolean = false
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[S1[_] >: Nothing, T[_]](fK: S1 ~> T): Record[T, Unit] = this

  final private[otter] case class Field[S[_], A](name: String, codec: Reference[S, A], metadata: Metadata)
      extends Record[S, A]:
    override def isOptional: Boolean = false
    override def fields: Chain[(String, Reference[S, A])] = Chain.one((name, codec))
    override def modifyMetadata(f: Metadata => Metadata): Record[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Record[T, A] = copy(codec = codec.mapK(fK))

  final private[otter] case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.{fields, isOptional, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Record[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Optional[S[_], A](self: Record[S, A]) extends Record[S, Option[A]]:
    export self.{fields, metadata}
    override def isOptional: Boolean = true
    override def modifyMetadata(f: Metadata => Metadata): Record[S, Option[A]] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Record[T, Option[A]] = copy(self = self.mapK(fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Record[S, A],
      right: Record[S, B],
      metadata: Metadata
  ) extends Record[S, (A, B)]:
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def fields: Chain[(String, Reference[S, ?])] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, (A, B)] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Record[T, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))
