package io.taig.otter

import cats.syntax.all.*

trait UnionInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def branch[A](name: String, codec: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def discriminator: Option[Discriminator]
    def orElse[B](codec: Self[B]): Self[Either[A, B]]
    final def :+[B](codec: Self[B]): Self[Either[A, B]] = orElse(codec)
    final def +:[B](codec: Self[B]): Self[Either[B, A]] = codec.orElse(self)
    def keyed: Self[A]
    def merged(discriminator: Discriminator.Merged): Self[A]
    final def merged: Self[A] = merged(discriminator = Discriminator.Merged.Default)
    def explicit(discriminator: Discriminator.Explicit): Self[A]
    final def explicit: Self[A] = explicit(discriminator = Discriminator.Explicit.Default)

object UnionInvariant:
  def apply[Self[_], Branch[_]](
      lift: [A] => (codec: Union[Branch, A]) => Self[A],
      extract: [A] => (codec: Self[A]) => Union[Branch, A]
  ): UnionInvariant[Self, Branch] = new UnionInvariant[Self, Branch]:
    override def branch[A](name: String, codec: => Branch[A]): Self[A] =
      lift(Union.Untagged.Branch(name, codec = Reference.later(codec), metadata = Metadata.Empty))

    extension [A](self: Self[A])
      override def imap[B](f: A => B)(g: B => A): Self[B] =
        lift(extract(self).imap(f)(g))
      override def metadata: Metadata = extract(self).metadata
      override def modifyMetadata(f: Metadata => Metadata): Self[A] =
        lift(extract(self).modifyMetadata(f))
      override def discriminator: Option[Discriminator] = extract(self) match
        case codec: Union.Untagged[?, ?]                    => none
        case Union.Tagged.Keyed(untagged)                   => Discriminator.Keyed.some
        case Union.Tagged.Merged(untagged, discriminator)   => discriminator.some
        case Union.Tagged.Explicit(untagged, discriminator) => discriminator.some
      override def keyed: Self[A] = lift(extract(self).keyed)
      override def merged(discriminator: Discriminator.Merged): Self[A] =
        lift(extract(self).merged(discriminator))
      override def explicit(discriminator: Discriminator.Explicit): Self[A] =
        lift(extract(self).explicit(discriminator))
      override def orElse[B](codec: Self[B]): Self[Either[A, B]] =
        lift(extract(self).orElse(extract(codec)))
