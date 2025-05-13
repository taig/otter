package io.taig.otter

import cats.syntax.all.*
import cats.~>
import cats.data.NonEmptyChain

sealed abstract class Sum[+S[_], +T[_], A] extends Product with Serializable:
  def branches: NonEmptyChain[Branch[S, T, ?]]

  def discriminator: Discriminator
  def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, A]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Sum[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Sum[S, T, B] = Sum.Modify(self = this, f, g)
  def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Sum[S, U, A]

object Sum:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Sum[S, T, A], f: A => B, g: B => A)
      extends Sum[S, T, B]:
    export self.{branches, discriminator, metadata}
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, B] =
      copy(self = self.modifyDiscriminator(f))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Sum[S, U, B] = copy(self = self.mapK(fK))

  final private[otter] case class OrElse[S[_], T[_], A, B](
      left: Sum[S, T, A],
      right: Sum[S, T, B],
      discriminator: Discriminator,
      metadata: Metadata
  ) extends Sum[S, T, Either[A, B]]:
    override def branches: NonEmptyChain[Branch[S, T, ?]] = left.branches ++ right.branches
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, Either[A, B]] =
      copy(discriminator = f(discriminator))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, T, Either[A, B]] =
      copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Sum[S, U, Either[A, B]] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  final private[otter] case class Root[S[_], T[_], A](
      branch: Branch[S, T, A],
      discriminator: Discriminator,
      metadata: Metadata
  ) extends Sum[S, T, A]:
    override def branches: NonEmptyChain[Branch[S, T, ?]] = NonEmptyChain.one(branch)
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, A] =
      copy(discriminator = f(discriminator))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, T, A] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Sum[S, U, A] = copy(branch = branch.mapK(fK))
