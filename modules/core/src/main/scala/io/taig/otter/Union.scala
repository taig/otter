package io.taig.otter

import cats.syntax.all.*
import cats.~>
import cats.data.NonEmptyChain

sealed abstract class Union[+S[_], A] extends Product with Serializable:
  def codecs: NonEmptyChain[Reference[S, ?]]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Union[S, A]

  final def imap[B](f: A => B)(g: B => A): Union[S, B] = Union.Modify(self = this, f, g)

  final def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union[S1, Either[A, B]] =
    Union.OrElse(left = this, right = codec, metadata = Metadata.Empty)

  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, A]

object Union:
  final private[otter] case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.{codecs, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Union[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class OrElse[S[_], A, B](
      left: Union[S, A],
      right: Union[S, B],
      metadata: Metadata
  ) extends Union[S, Either[A, B]]:
    override def codecs: NonEmptyChain[Reference[S, ?]] = left.codecs ++ right.codecs
    override def modifyMetadata(f: Metadata => Metadata): Union[S, Either[A, B]] =
      copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, Either[A, B]] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  final private[otter] case class Root[S[_], A](codec: Reference[S, A], metadata: Metadata) extends Union[S, A]:
    override def codecs: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(codec)
    override def modifyMetadata(f: Metadata => Metadata): Union[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, A] = copy(codec = codec.mapK(fK))
