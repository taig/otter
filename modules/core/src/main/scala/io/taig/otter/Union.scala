package io.taig.otter

import cats.data.Chain
import cats.~>

sealed abstract class Union[+S[_], A] extends Codec[S, A]:
  def branches: Chain[Reference[S, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Union[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union[T, A]
  override def imap[B](f: A => B)(g: B => A): Union[S, B]

  def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union[S1, Either[A, B]]

  def untagged: Union.Untagged[S, A]
  def keyed: Union.Tagged[S, A]
  def merged(discriminator: Discriminator.Merged): Union.Tagged[S, A]
  def nested(discriminator: Discriminator.Nested): Union.Tagged[S, A]

object Union:
  sealed abstract class Untagged[+S[_], A] extends Union[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, A]
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Untagged[T, A]
    final override def imap[B](f: A => B)(g: B => A): Union.Untagged[S, B] = Untagged.Modify(self = this, f, g)

    override def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union.Untagged[S1, Either[A, B]] =
      Untagged.OrElse(left = this, right = codec.untagged, metadata = Metadata.Empty)
    final override def untagged: Union.Untagged[S, A] = this
    final override def keyed: Union.Tagged[S, A] = Tagged.Keyed(untagged = this)
    final override def merged(discriminator: Discriminator.Merged): Union.Tagged[S, A] =
      Tagged.Merged(untagged = this, discriminator)
    final override def nested(discriminator: Discriminator.Nested): Union.Tagged[S, A] =
      Tagged.Nested(untagged = this, discriminator)

  object Untagged:
    final private[otter] case class OrElse[S[_], A, B](
        left: Union.Untagged[S, A],
        right: Union.Untagged[S, B],
        metadata: Metadata
    ) extends Union.Untagged[S, Either[A, B]]:
      override def branches: Chain[Reference[S, ?]] = left.branches ++ right.branches
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, Either[A, B]] =
        copy(metadata = f(metadata))
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Untagged[T, Either[A, B]] =
        copy(left = left.mapK(fK), right = right.mapK(fK))

    final private[otter] case class Root[S[_], A](branch: Reference[S, A], metadata: Metadata)
        extends Union.Untagged[S, A]:
      override def branches: Chain[Reference[S, A]] = Chain.one(branch)
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, A] = copy(metadata = f(metadata))
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Untagged[T, A] = copy(branch = branch.mapK(fK))

    final private[otter] case class Modify[S[_], A, B](self: Union.Untagged[S, A], f: A => B, g: B => A)
        extends Union.Untagged[S, B]:
      export self.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, B] =
        copy(self = self.modifyMetadata(f))
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Untagged[T, B] = copy(self = self.mapK(fK))

  sealed abstract class Tagged[+S[_], A] extends Union[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A]
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Tagged[T, A]
    override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B]
    override def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union.Tagged[S1, Either[A, B]]

  object Tagged:
    final private[otter] case class Keyed[S[_], A](untagged: Union.Untagged[S, A]) extends Union.Tagged[S, A]:
      export untagged.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Tagged[T, A] = copy(untagged = untagged.mapK(fK))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B] = copy(untagged = untagged.imap(f)(g))
      override def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Tagged[S1, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, A] = this
      override def merged(discriminator: Discriminator.Merged): Union.Tagged[S, A] = Merged(untagged, discriminator)
      override def nested(discriminator: Discriminator.Nested): Union.Tagged[S, A] = Nested(untagged, discriminator)

    final private[otter] case class Merged[S[_], A](
        untagged: Union.Untagged[S, A],
        discriminator: Discriminator.Merged
    ) extends Union.Tagged[S, A]:
      export untagged.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Tagged[T, A] = copy(untagged = untagged.mapK(fK))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B] = copy(untagged = untagged.imap(f)(g))
      override def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union.Tagged[S1, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, A] = Keyed(untagged)
      override def merged(discriminator: Discriminator.Merged): Union.Tagged[S, A] = copy(discriminator = discriminator)
      override def nested(discriminator: Discriminator.Nested): Union.Tagged[S, A] = Nested(untagged, discriminator)

    final private[otter] case class Nested[S[_], T[_], A](
        untagged: Union.Untagged[S, A],
        discriminator: Discriminator.Nested
    ) extends Union.Tagged[S, A]:
      export untagged.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Union.Tagged[T, A] = copy(untagged = untagged.mapK(fK))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B] = copy(untagged = untagged.imap(f)(g))
      override def orElse[S1[a] >: S[a], B](codec: Union[S1, B]): Union.Tagged[S1, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, A] = Keyed(untagged)
      override def merged(discriminator: Discriminator.Merged): Union.Tagged[S, A] = Merged(untagged, discriminator)
      override def nested(discriminator: Discriminator.Nested): Union.Tagged[S, A] = copy(discriminator = discriminator)
