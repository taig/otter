package io.taig.otter

import cats.~>
import cats.arrow.FunctionK

sealed abstract class Branch[+S[_], +T[_], A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, A] = ???

object Branch:
  final private[otter] case class Explicit[+S[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[S, B],
      metadata: Metadata
  ) extends Branch[S, S, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, S, B] = copy(metadata = f(metadata))
    // override def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Branch[U, U, B] = copy(key = key.mapK(fK), value = value.mapK(fK))

  final private[otter] case class Keyed[+S[_], +T[_], A](metadata: Metadata) extends Branch[S, T, A]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, A] = ???

  final private[otter] case class Merged[+S[_], A](metadata: Metadata) extends Branch[S, S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Branch[S, U, A] = ???

  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Branch[S, U, B] = copy(self = self.mapK(fK))

  // final private[otter] case class Untagged[+S[_], A](name: String, codec: Reference[S, A], metadata: Metadata)
  //     extends Branch[S, S, A]:
  //   override def modifyMetadata(f: Metadata => Metadata): Branch[S, S, A] = copy(metadata = f(metadata))
  //   override def mapK[S1[a] >: S[a], U[_]](fK: T1 ~> U): Branch[S, U, A] = ???
