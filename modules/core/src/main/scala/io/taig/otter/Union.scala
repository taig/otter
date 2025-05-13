package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import cats.~>

sealed abstract class Union[+S[_], +T[_], A]:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Union[S, T, A]
  def branches: Chain[Branch[S, T, ?]]
  def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Union[S, U, A]

  final def imap[B](f: A => B)(g: B => A): Union[S, T, B] = ???

  final def orElse[S1[a] >: S[a], T1[a] >: T[a], B](codec: Union[S1, T1, B]): Union[S1, T1, Either[A, B]] = ???

object Union:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Union[S, T, A], f: A => B, g: B => A)
      extends Union[S, T, B]:
    export self.{branches, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Union[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Union[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], T[_], A](branch: Branch[S, T, A], metadata: Metadata)
      extends Union[S, T, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union[S, T, A] = copy(metadata = f(metadata))
    override def branches: Chain[Branch[S, T, ?]] = Chain.one(branch)
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Union[S, U, A] = copy(branch = branch.mapK(fK))
