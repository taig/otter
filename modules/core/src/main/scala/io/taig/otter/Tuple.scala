package io.taig.otter

import scala.Tuple as STuple
import cats.~>
import cats.arrow.FunctionK
import io.taig.otter.Tuple

sealed abstract class Tuple[+S[_], A] extends Codec[S, A]:
  def codecs: Vector[Reference[S, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, A]
  final override def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)

object Tuple:
  final private[otter] case class Empty(metadata: Metadata) extends Tuple[Nothing, EmptyTuple]:
    override def codecs: Vector[Nothing] = Vector.empty
    override def mapK[S1[a] >: Nothing, T[_]](fK: S1 ~> T): Tuple[T, EmptyTuple] = this
    override def modifyMetadata(f: Metadata => Metadata): Tuple[Nothing, EmptyTuple] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.{codecs, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A](codec: Reference[S, A], metadata: Metadata) extends Tuple[S, A]:
    override def codecs: Vector[Reference[S, A]] = Vector(codec)
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, A] = copy(codec = codec.mapK(fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Tuple[S, A],
      right: Tuple[S, B],
      metadata: Metadata
  ) extends Tuple[S, (A, B)]:
    override def codecs: Vector[Reference[S, ?]] = left.codecs ++ right.codecs
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, (A, B)] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))
