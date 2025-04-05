package io.taig.otter

import cats.syntax.all.*

trait UnionInvariant[Self[_], Value[_]] extends CoproductInvariant[Self]:
  def branch[A](name: String, codec: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def discriminator: Option[Discriminator]
    def untagged: Self[A]
    def keyed: Self[A]
    def merged(discriminator: Discriminator.Merged): Self[A]
    final def merged: Self[A] = merged(discriminator = Discriminator.Merged.Default)
    def explicit(discriminator: Discriminator.Explicit): Self[A]
    final def explicit: Self[A] = explicit(discriminator = Discriminator.Explicit.Default)

object UnionInvariant:
  trait Untagged[Self[_], Value[_]] extends CoproductInvariant[Self]:
    def branch[A](name: String, codec: => Value[A]): Self[A]

  object Untagged:
    def apply[Self[_], Value[_]](
        lift: [A] => (codec: Union.Untagged[Value, A]) => Self[A],
        extract: [A] => (codec: Self[A]) => Union.Untagged[Value, A]
    ): UnionInvariant.Untagged[Self, Value] = new Untagged[Self, Value]:
      override def branch[A](name: String, codec: => Value[A]): Self[A] =
        lift(Union.Untagged.Branch(name, codec = Reference.later(codec), metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def orElse[B](codec: Self[B]): Self[Either[A, B]] =
          lift(extract(self).orElse(extract(codec)))

  trait Tagged[Self[_], Value[_]] extends CoproductInvariant[Self]:
    def branch[A](name: String, codec: => Value[A]): Self[A]

    extension [A](self: Self[A])
      def discriminator: Discriminator
      def keyed: Self[A]
      def merged(discriminator: Discriminator.Merged): Self[A]
      final def merged: Self[A] = merged(discriminator = Discriminator.Merged.Default)
      def explicit(discriminator: Discriminator.Explicit): Self[A]
      final def explicit: Self[A] = explicit(discriminator = Discriminator.Explicit.Default)

  def apply[Self[_], Value[_]](
      lift: [A] => (codec: Union[Value, A]) => Self[A],
      extract: [A] => (codec: Self[A]) => Union[Value, A]
  ): UnionInvariant[Self, Value] = new UnionInvariant[Self, Value]:
    override def branch[A](name: String, codec: => Value[A]): Self[A] =
      lift(Union.Untagged.Branch(name, codec = Reference.later(codec), metadata = Metadata.Empty))

    extension [A](self: Self[A])
      override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
      override def metadata: Metadata = extract(self).metadata
      override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
      override def discriminator: Option[Discriminator] = extract(self) match
        case codec: Union.Untagged[?, ?]                    => none
        case Union.Tagged.Keyed(untagged)                   => Discriminator.Keyed.some
        case Union.Tagged.Merged(untagged, discriminator)   => discriminator.some
        case Union.Tagged.Explicit(untagged, discriminator) => discriminator.some
      override def untagged: Self[A] = lift(extract(self).untagged)
      override def keyed: Self[A] = lift(extract(self).keyed)
      override def merged(discriminator: Discriminator.Merged): Self[A] =
        lift(extract(self).merged(discriminator))
      override def explicit(discriminator: Discriminator.Explicit): Self[A] =
        lift(extract(self).explicit(discriminator))
      override def orElse[B](codec: Self[B]): Self[Either[A, B]] =
        lift(extract(self).orElse(extract(codec)))
