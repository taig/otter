package io.taig.otter

import cats.data.NonEmptyChain
import io.taig.otter.Metadata
import io.taig.otter.schema.SumSchema

sealed abstract class Sum[+S[_], +T[_], A] extends Product with Serializable:
  def branches: NonEmptyChain[Branch[S, T, ?]]

  def discriminator: Discriminator
  def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, A]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Sum[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Sum[S, T, B] = Sum.Modify(self = this, f, g)
  final def orElse[S1[a] >: S[a], T1[a] >: T[a], B](schema: Sum[S1, T1, B]): Sum[S1, T1, Either[A, B]] =
    Sum.OrElse(left = this, right = schema, discriminator, metadata = Metadata.Empty)
  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Sum[S, U, A]

object Sum:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Sum[S, T, A], f: A => B, g: B => A)
      extends Sum[S, T, B]:
    export self.{branches, discriminator, metadata}
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, B] =
      copy(self = self.modifyDiscriminator(f))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Sum[S, U, B] = copy(self = self.mapK[T1, U](fK))

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
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Sum[S, U, Either[A, B]] =
      copy(left = left.mapK[T1, U](fK), right = right.mapK[T1, U](fK))

  final private[otter] case class Root[S[_], T[_], A](
      branch: Branch[S, T, A],
      discriminator: Discriminator,
      metadata: Metadata
  ) extends Sum[S, T, A]:
    override def branches: NonEmptyChain[Branch[S, T, A]] = NonEmptyChain.one(branch)
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, T, A] =
      copy(discriminator = f(discriminator))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, T, A] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Sum[S, U, A] =
      copy(branch = branch.mapK[T1, U](fK))

  given [Key[_], Value[_]]: SumSchema[Sum[Key, Value, *], Key, Value] with
    override def sum[A](branch: Branch[Key, Value, A]): Sum[Key, Value, A] =
      Root(branch, discriminator = Discriminator.Keyed, metadata = Metadata.Empty)

    extension [A](self: Sum[Key, Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Sum[Key, Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Sum[Key, Value, B] = self.imap(f)(g)
      override def orElse[B](schema: Sum[Key, Value, B]): Sum[Key, Value, Either[A, B]] = self.orElse(schema)
