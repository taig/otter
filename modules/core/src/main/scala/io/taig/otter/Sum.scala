package io.taig.otter

import cats.data.NonEmptyChain
import io.taig.otter.Metadata
import io.taig.otter.schema.SumSchema

sealed abstract class Sum[+S[_], A] extends Product with Serializable:
  def branches: NonEmptyChain[Reference[S, ?]]

  def discriminator: Discriminator
  def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, A]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Sum[S, A]

  final def imap[B](f: A => B)(g: B => A): Sum[S, B] = Sum.Modify(self = this, f, g)
  final def orElse[S1[a] >: S[a], B](schema: Sum[S1, B]): Sum[S1, Either[A, B]] =
    Sum.OrElse(left = this, right = schema, discriminator, metadata = Metadata.Empty)
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Sum[T, A]

object Sum:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Sum[S, A], f: A => B, g: B => A) extends Sum[S, B]:
    export self.{branches, discriminator, metadata}
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, B] =
      copy(self = self.modifyDiscriminator(f))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Sum[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class OrElse[S[_], A, B](
      left: Sum[S, A],
      right: Sum[S, B],
      discriminator: Discriminator,
      metadata: Metadata
  ) extends Sum[S, Either[A, B]]:
    override def branches: NonEmptyChain[Reference[S, ?]] = left.branches ++ right.branches
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, Either[A, B]] =
      copy(discriminator = f(discriminator))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, Either[A, B]] =
      copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Sum[T, Either[A, B]] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](
      branch: Reference[S, A],
      discriminator: Discriminator,
      metadata: Metadata
  ) extends Sum[S, A]:
    override def branches: NonEmptyChain[Reference[S, A]] = NonEmptyChain.one(branch)
    override def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, A] =
      copy(discriminator = f(discriminator))
    override def modifyMetadata(f: Metadata => Metadata): Sum[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Sum[T, A] =
      copy(branch = branch.mapK[S1, T](fK))

  given [Branch[_]]: SumSchema[Sum[Branch, *], Branch] with
    override def lift[A](branch: => Branch[A]): Sum[Branch, A] =
      Root(branch = Reference.later(branch), discriminator = Discriminator.Keyed, metadata = Metadata.Empty)

    override def imap[A, B](fa: Sum[Branch, A])(f: A => B)(g: B => A): Sum[Branch, B] = fa.imap(f)(g)

    extension [A](self: Sum[Branch, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Sum[Branch, A] = self.modifyMetadata(f)
      override def orElse[B](schema: Sum[Branch, B]): Sum[Branch, Either[A, B]] = self.orElse(schema)
