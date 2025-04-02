package io.taig.otter

import cats.data.Chain
import cats.Invariant

sealed abstract class Union[+S[_], +T[_], A] extends Codec[T, A]:
  def branches: Chain[Branch[S, T, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Union[S, T, A]
  override def imap[B](f: A => B)(g: B => A): Union[S, T, B]

  def orElse[S1[a] >: S[a], T1[a] >: T[a], B](codec: Union[S1, T1, B]): Union[S1, T1, Either[A, B]]

  def :+[S1[a] >: S[a], T1[a] >: T[a], B](branch: Branch[S1, T1, B]): Union[S1, T1, Either[A, B]]

  def untagged: Union.Untagged[S, T, A]
  def keyed: Union.Tagged[S, T, A]
  def merged: Union.Tagged[S, T, A]
  def nested: Union.Tagged[S, T, A]

object Union:
  sealed abstract class Untagged[+S[_], +T[_], A] extends Union[S, T, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, T, A]
    final override def imap[B](f: A => B)(g: B => A): Union.Untagged[S, T, B] = Untagged.Modify(self = this, f, g)

    override def orElse[S1[a] >: S[a], T1[a] >: T[a], B](
        codec: Union[S1, T1, B]
    ): Union.Untagged[S1, T1, Either[A, B]] =
      Untagged.OrElse(left = this, right = codec.untagged, metadata = Metadata.Empty)
    override def :+[S1[a] >: S[a], T1[a] >: T[a], B](branch: Branch[S1, T1, B]): Union.Untagged[S1, T1, Either[A, B]] =
      orElse(codec = branch.toUnion)
    final override def untagged: Union.Untagged[S, T, A] = this
    final override def keyed: Union.Tagged[S, T, A] = Tagged.Keyed(untagged = this)
    final override def merged: Union.Tagged[S, T, A] =
      Tagged.Merged(untagged = this, discriminator = Discriminator.Merged.Default)
    final override def nested: Union.Tagged[S, T, A] =
      Tagged.Nested(untagged = this, discriminator = Discriminator.Nested.Default)

  object Untagged:
    final private[otter] case class OrElse[S[_], T[_], A, B](
        left: Union.Untagged[S, T, A],
        right: Union.Untagged[S, T, B],
        metadata: Metadata
    ) extends Union.Untagged[S, T, Either[A, B]]:
      override def branches: Chain[Branch[S, T, ?]] = left.branches ++ right.branches
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, T, Either[A, B]] =
        copy(metadata = f(metadata))

    final private[otter] case class Root[S[_], T[_], A](branch: Branch[S, T, A], metadata: Metadata)
        extends Union.Untagged[S, T, A]:
      override def branches: Chain[Branch[S, T, A]] = Chain.one(branch)
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, T, A] = copy(metadata = f(metadata))

    final private[otter] case class Modify[S[_], T[_], A, B](self: Union.Untagged[S, T, A], f: A => B, g: B => A)
        extends Union.Untagged[S, T, B]:
      export self.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, T, B] =
        copy(self = self.modifyMetadata(f))

    extension [S[_], T[_], A <: Matchable](self: Union.Untagged[S, T, A])
      inline def |[S1[a] >: S[a], T1[a] >: T[a], B <: Matchable](
          branch: Branch[S1, T1, B]
      ): Union.Untagged[S1, T1, A | B] =
        (self :+ branch).imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

    given [S[_], T[_]]: Invariant[Union.Untagged[S, T, *]] with
      override def imap[A, B](fa: Union.Untagged[S, T, A])(f: A => B)(g: B => A): Union.Untagged[S, T, B] =
        fa.imap(f)(g)

  sealed abstract class Tagged[+S[_], +T[_], A] extends Union[S, T, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, T, A]
    override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, T, B]
    override def orElse[S1[a] >: S[a], T1[a] >: T[a], B](codec: Union[S1, T1, B]): Union.Tagged[S1, T1, Either[A, B]]
    override def :+[S1[a] >: S[a], T1[a] >: T[a], B](branch: Branch[S1, T1, B]): Union.Tagged[S1, T1, Either[A, B]] =
      orElse(codec = branch.toUnion)

  object Tagged:
    final private[otter] case class Keyed[S[_], T[_], A](untagged: Union.Untagged[S, T, A])
        extends Union.Tagged[S, T, A]:
      export untagged.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, T, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, T, B] = copy(untagged = untagged.imap(f)(g))
      override def orElse[S1[a] >: S[a], T1[a] >: T[a], B](codec: Union[S1, T1, B]): Tagged[S1, T1, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, T, A] = this
      override def merged: Union.Tagged[S, T, A] =
        Merged(untagged, discriminator = Discriminator.Merged.Default)
      override def nested: Union.Tagged[S, T, A] =
        Nested(untagged, discriminator = Discriminator.Nested.Default)

    final private[otter] case class Merged[S[_], T[_], A](
        untagged: Union.Untagged[S, T, A],
        discriminator: Discriminator.Merged
    ) extends Union.Tagged[S, T, A]:
      export untagged.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, T, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, T, B] = copy(untagged = untagged.imap(f)(g))
      override def orElse[S1[a] >: S[a], T1[a] >: T[a], B](
          codec: Union[S1, T1, B]
      ): Union.Tagged[S1, T1, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, T, A] = Keyed(untagged)
      override def merged: Union.Tagged[S, T, A] = this
      override def nested: Union.Tagged[S, T, A] = Nested(untagged, discriminator = Discriminator.Nested.Default)

    final private[otter] case class Nested[S[_], T[_], A](
        untagged: Union.Untagged[S, T, A],
        discriminator: Discriminator.Nested
    ) extends Union.Tagged[S, T, A]:
      export untagged.{branches, metadata}
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, T, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, T, B] = copy(untagged = untagged.imap(f)(g))
      override def orElse[S1[a] >: S[a], T1[a] >: T[a], B](
          codec: Union[S1, T1, B]
      ): Union.Tagged[S1, T1, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, T, A] = Keyed(untagged)
      override def merged: Union.Tagged[S, T, A] =
        Merged(untagged, discriminator = Discriminator.Merged.Default)
      override def nested: Union.Tagged[S, T, A] = this

    given [S[_], T[_]]: Invariant[Union.Tagged[S, T, *]] with
      override def imap[A, B](fa: Union.Tagged[S, T, A])(f: A => B)(g: B => A): Union.Tagged[S, T, B] = fa.imap(f)(g)

  extension [S[_], T[_], A <: Matchable](self: Union[S, T, A])
    inline def |[B <: Matchable](branch: Branch[S, T, B]): Union[S, T, A | B] =
      (self :+ branch).imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  given [S[_], T[_]]: Invariant[Union[S, T, *]] with
    override def imap[A, B](fa: Union[S, T, A])(f: A => B)(g: B => A): Union[S, T, B] = fa.imap(f)(g)
