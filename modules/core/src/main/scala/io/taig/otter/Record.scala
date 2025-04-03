package io.taig.otter

import cats.data.Chain
import cats.~>

sealed abstract class Record[+S[_], +T[_], A] extends Codec[T, A]:
  def fields: Chain[Field[S, T, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Record[S, T, A]
  override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, A]
  def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, T, A]
  final override def imap[B](f: A => B)(g: B => A): Record[S, T, B] = Record.Modify(self = this, f, g)
  final def zip[S1[a] >: S[a], T1[a] >: T[a], B](codec: Record[S1, T1, B]): Record[S1, T1, (A, B)] =
    Record.Zip(left = this, right = codec, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Nothing, Unit] = copy(metadata = f(metadata))
    override def mapK[T1[_] >: Nothing, U[_]](fK: T1 ~> U): Record[Nothing, U, Unit] = this
    override def leftMapK[S1[_] >: Nothing, U[_]](fK: S1 ~> U): Record[U, Nothing, Unit] = this

  final private[otter] case class Modify[S[_], T[_], A, B](self: Record[S, T, A], f: A => B, g: B => A)
      extends Record[S, T, B]:
    export self.{fields, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, B] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, T, B] = copy(self = self.leftMapK(fK))

  final private[otter] case class Root[S[_], T[_], A](field: Field[S, T, A], metadata: Metadata)
      extends Record[S, T, A]:
    override def fields: Chain[Field[S, T, A]] = Chain.one(field)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, A] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, A] = copy(field = field.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, T, A] = copy(field = field.leftMapK(fK))

  final private[otter] case class Zip[S[_], T[_], A, B](
      left: Record[S, T, A],
      right: Record[S, T, B],
      metadata: Metadata
  ) extends Record[S, T, (A, B)]:
    override def fields: Chain[Field[S, T, ?]] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, (A, B)] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Record[S, U, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Record[U, T, (A, B)] =
      copy(left = left.leftMapK(fK), right = right.leftMapK(fK))
