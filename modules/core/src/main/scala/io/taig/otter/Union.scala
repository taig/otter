package io.taig.otter

sealed abstract class Union[+S[_], A] extends Codec[S, A]:
  override def modifyMetadata(f: Metadata => Metadata): Union[S, A]
  override def imap[B](f: A => B)(g: B => A): Union[S, B]

  def orElse[T[a] >: S[a], B](codec: Union[T, B]): Union[T, Either[A, B]]

  def :+[T[a] >: S[a], B](branch: Branch[T, B]): Union[T, Either[A, B]]

  def untagged: Union.Untagged[S, A]
  def keyed: Union.Tagged[S, A]
  def merged: Union.Tagged[S, A]
  def nested: Union.Tagged[S, A]

object Union:
  sealed abstract class Untagged[+S[_], A] extends Union[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, A]
    final override def imap[B](f: A => B)(g: B => A): Union.Untagged[S, B] = Untagged.Modify(self = this, f, g)
    override def orElse[T[a] >: S[a], B](codec: Union[T, B]): Union.Untagged[T, Either[A, B]] =
      Untagged.OrElse(left = this, right = codec.untagged, metadata = Metadata.Empty)
    override def :+[T[a] >: S[a], B](branch: Branch[T, B]): Union.Untagged[T, Either[A, B]] =
      orElse(codec = branch.toUnion)
    final override def untagged: Union.Untagged[S, A] = this
    final override def keyed: Union.Tagged[S, A] = Tagged.Keyed(untagged = this)
    final override def merged: Union.Tagged[S, A] =
      Tagged.Merged(untagged = this, discriminator = Discriminator.Merged.Default)
    final override def nested: Union.Tagged[S, A] =
      Tagged.Nested(untagged = this, discriminator = Discriminator.Nested.Default)

  object Untagged:
    final private[otter] case class OrElse[S[_], A, B](
        left: Union.Untagged[S, A],
        right: Union.Untagged[S, B],
        metadata: Metadata
    ) extends Union.Untagged[S, Either[A, B]]:
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, Either[A, B]] =
        copy(metadata = f(metadata))

    final private[otter] case class Root[S[_], A](branch: Branch[S, A], metadata: Metadata)
        extends Union.Untagged[S, A]:
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, A] = copy(metadata = f(metadata))

    final private[otter] case class Modify[S[_], A, B](self: Union.Untagged[S, A], f: A => B, g: B => A)
        extends Union.Untagged[S, B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, B] = copy(self = self.modifyMetadata(f))

    extension [S[_], A <: Matchable](self: Union.Untagged[S, A])
      inline def |[B <: Matchable](branch: Branch[S, B]): Union.Untagged[S, A | B] =
        (self :+ branch).imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

  sealed abstract class Tagged[+S[_], A] extends Union[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A]
    override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B]
    override def orElse[T[a] >: S[a], B](codec: Union[T, B]): Union.Tagged[T, Either[A, B]]
    override def :+[T[a] >: S[a], B](branch: Branch[T, B]): Union.Tagged[T, Either[A, B]] =
      orElse(codec = branch.toUnion)

  object Tagged:
    final private[otter] case class Keyed[S[_], A](untagged: Union.Untagged[S, A]) extends Union.Tagged[S, A]:
      export untagged.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[T[a] >: S[a], B](codec: Union[T, B]): Union.Tagged[T, Either[A, B]] =
        Keyed(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, A] = this
      override def merged: Union.Tagged[S, A] =
        Merged(untagged, discriminator = Discriminator.Merged.Default)
      override def nested: Union.Tagged[S, A] =
        Nested(untagged, discriminator = Discriminator.Nested.Default)

    final private[otter] case class Merged[S[_], A](
        untagged: Union.Untagged[S, A],
        discriminator: Discriminator.Merged
    ) extends Union.Tagged[S, A]:
      export untagged.metadata
      override def modifyMetadata(
          f: Metadata => Metadata
      ): Union.Tagged[S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[T[a] >: S[a], B](codec: Union[T, B]): Union.Tagged[T, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, A] = Keyed(untagged)
      override def merged: Union.Tagged[S, A] = this
      override def nested: Union.Tagged[S, A] =
        Nested(untagged, discriminator = Discriminator.Nested.Default)

    final private[otter] case class Nested[S[_], A](
        untagged: Union.Untagged[S, A],
        discriminator: Discriminator.Nested
    ) extends Union.Tagged[S, A]:
      export untagged.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[S, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[T[a] >: S[a], B](codec: Union[T, B]): Union.Tagged[T, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[S, A] = Keyed(untagged)
      override def merged: Union.Tagged[S, A] =
        Merged(untagged, discriminator = Discriminator.Merged.Default)
      override def nested: Union.Tagged[S, A] = this

  extension [S[_], A <: Matchable](self: Union[S, A])
    inline def |[B <: Matchable](branch: Branch[S, B]): Union[S, A | B] =
      (self :+ branch).imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  given [S[_]]: CodecInvariant[Union[S, *]] with
    override def imap[A, B](fa: Union[S, A])(f: A => B)(g: B => A): Union[S, B] = fa.imap(f)(g)
