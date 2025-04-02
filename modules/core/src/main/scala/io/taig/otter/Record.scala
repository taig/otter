package io.taig.otter

import cats.data.Chain
import cats.Invariant

sealed abstract class Record[+S[_], +T[_], A] extends Codec[S, A]:
  def fields: Chain[Field[S, T, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Record[S, T, A]
  final override def imap[B](f: A => B)(g: B => A): Record[S, T, B] = Record.Modify(self = this, f, g)
  final def zip[S1[a] >: S[a], T1[a] >: T[a], B](codec: Record[S1, T1, B]): Record[S1, T1, (A, B)] =
    Record.Zip(left = this, right = codec, metadata = Metadata.Empty)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Record[S, T, A], f: A => B, g: B => A)
      extends Record[S, T, B]:
    export self.{fields, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S[_], T[_], A](field: Field[S, T, A], metadata: Metadata)
      extends Record[S, T, A]:
    override def fields: Chain[Field[S, T, A]] = Chain.one(field)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, A] = copy(metadata = f(metadata))

  final private[otter] case class Zip[S[_], T[_], A, B](
      left: Record[S, T, A],
      right: Record[S, T, B],
      metadata: Metadata
  ) extends Record[S, T, (A, B)]:
    override def fields: Chain[Field[S, T, ?]] = left.fields ++ right.fields
    override def modifyMetadata(f: Metadata => Metadata): Record[S, T, (A, B)] = copy(metadata = f(metadata))

  given [S[_], T[_]]: Invariant[Record[S, T, *]] with
    override def imap[A, B](fa: Record[S, T, A])(f: A => B)(g: B => A): Record[S, T, B] = fa.imap(f)(g)
