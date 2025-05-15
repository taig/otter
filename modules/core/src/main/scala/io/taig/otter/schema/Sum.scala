package io.taig.otter.schema

import io.taig.otter.Discriminator
import io.taig.otter.Metadata
import cats.~>
import cats.data.NonEmptyChain
import io.taig.otter.Reference
import io.taig.otter.Shape

sealed abstract class Sum[+S[_], A] extends Product with Serializable:
  def branches: NonEmptyChain[Reference[S, ?]]

  def discriminator: Discriminator
  def modifyDiscriminator(f: Discriminator => Discriminator): Sum[S, A]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Sum[S, A]

  final def imap[B](f: A => B)(g: B => A): Sum[S, B] = Sum.Modify(self = this, f, g)
  final def orElse[S1[a] >: S[a], B](codec: Sum[S1, B]): Sum[S1, Either[A, B]] =
    Sum.OrElse(left = this, right = codec, discriminator, metadata = Metadata.Empty)
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Sum[T, A]

object Sum:
  final private[otter] case class Modify[S[_], A, B](self: Sum[S, A], f: A => B, g: B => A) extends Sum[S, B]:
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
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Sum[T, A] = copy(branch = branch.mapK[S1, T](fK))

  given [Value[_]]: Shape.Sum[Sum[Value, *], Value] = new Shape.Sum[Sum[Value, *], Value]:
    extension [A](self: Sum[Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Sum[Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Sum[Value, B] = self.imap(f)(g)
      override def orElse[B](schema: Sum[Value, B]): Sum[Value, Either[A, B]] = self.orElse(schema)
