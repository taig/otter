package io.taig.otter

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Data.Optional
import cats.Id as Identity

sealed abstract class Sum[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, O, A]:
  self =>

  def branches: NonEmptyChain[Branch[?, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A]

  override def modifyDefault(f: Option[A] => Option[A]): Sum[F, O, A]

  override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]

  override def optional: Sum[Data.Optional, O, Option[A]]

  def orElse[B](sum: Sum[?, ?, B]): Sum[Identity, O, Either[A, B]] = ???

  def untagged: Sum.Untagged[F, Data, A]

  def encode(a: A): O

object Sum:
  sealed abstract class Nested[+F[+a] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = ???
    override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Nested[F, O, B] = ???
    override def optional: Sum.Nested[Data.Optional, O, Option[A]] = ???
    override def orElse[B](sum: Sum[?, ?, B]): Sum.Nested[Identity, O, Either[A, B]] = ???
    override def untagged: Sum.Untagged[F, O, A]

  object Nested:
    def apply[O <: Data, A](branch: Branch[O, A], discriminator: Discriminator.Nested): Sum.Nested[Identity, O, A] =
      new Nested[Identity, O, A]:
        override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def untagged: Untagged[Identity, O, A] = Untagged(branch)
        override def decode(data: Data): Codec.Result[A] = ???
        override def encode(a: A): Data.Object[Data.String | O] =
          Data.Object.of(discriminator.value -> branch.encode(a), discriminator.identifier -> Data.String(branch.name))

  sealed abstract class Merged[+F[+a] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = ???
    override def modifyDefault(f: Option[A] => Option[A]): Sum.Merged[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???
    override def optional: Sum.Merged[Data.Optional, O, Option[A]] = ???
    override def orElse[B](sum: Sum[?, ?, B]): Sum.Merged[Identity, O, Either[A, B]] = ???
    override def untagged: Sum.Untagged[F, Data.Object[O], A]

  object Merged:
    def apply[O <: Data.Object[P], P <: Data, A](
        branch: Branch[O, A],
        discriminator: Discriminator.Merged
    ): Sum.Merged[Identity, P, A] = new Merged[Identity, P, A]:
      override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[A] = None
      override def untagged: Untagged[Identity, Data.Object[P], A] = Untagged(branch)
      override def decode(data: Data): Codec.Result[A] = ???
      override def encode(a: A): O & Data.Object[Data.String] =
        ??? // Data.Object.one(discriminator.identifier, Data.String(branch.name)) ++ branch.encode(a)

  sealed abstract class Keyed[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]:
    override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = ???
    override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = ???
    override def optional: Sum.Keyed[Data.Optional, O, Option[A]] = ???
    override def untagged: Sum.Untagged[F, O, A]

  object Keyed:
    def apply[O <: Data, A](branch: Branch[O, A]): Sum.Keyed[Identity, O, A] = new Keyed[Identity, O, A]:
      override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[A] = None
      override def untagged: Sum.Untagged[Identity, O, A] = Untagged(branch)
      override def decode(data: Data): Codec.Result[A] = ???
      override def encode(a: A): Data.Object[O] = Data.Object.one(branch.name, branch.encode(a))

  sealed abstract class Untagged[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = ???
    override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = ???
    override def optional: Sum.Untagged[Data.Optional, O, Option[A]] = ???
    final override def untagged: Sum.Untagged[F, O, A] = this

  object Untagged:
    def apply[O <: Data, A](branch: Branch[O, A]): Sum.Untagged[Identity, O, A] = new Untagged[Identity, O, A]:
      override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[A] = None
      override def decode(data: Data): Codec.Result[A] = branch.decode(data)
      override def encode(a: A): O = branch.encode(a)
