package io.taig.otter

import cats.syntax.all.*

trait UnionDsl[Self[_], Value[_]]:
  protected def fromUnion[A](self: Union[Value, A]): Self[A]
  protected def toUnion[A](codec: Self[A]): Union[Value, A]

  final def branch[A](name: String, codec: => Value[A]): Self[A] =
    fromUnion(Union.Untagged.Branch(name, codec = Reference.later(codec), metadata = Metadata.Empty))

  extension [A](self: Self[A])
    final def discriminator: Option[Discriminator] = toUnion(self) match
      case codec: Union.Untagged[?, ?] => none
      case codec: Union.Tagged[?, ?]   => codec.discriminator.some

    final def keyed: Self[A] = fromUnion(toUnion(self).keyed)

    final def merged(discriminator: Discriminator.Merged): Self[A] = fromUnion(toUnion(self).merged(discriminator))
    final def merged: Self[A] = merged(discriminator = Discriminator.Merged.Default)

    final def explicit(discriminator: Discriminator.Explicit): Self[A] =
      fromUnion(toUnion(self).explicit(discriminator))
    final def explicit: Self[A] = explicit(discriminator = Discriminator.Explicit.Default)

object UnionDsl:
  trait Untagged[Self[_], -Value[_]]:
    protected def fromUnionUntagged[A](self: Union.Untagged[Value, A]): Self[A]

    final def branch[A](name: String, codec: => Value[A]): Self[A] =
      fromUnionUntagged(Union.Untagged.Branch(name, codec = Reference.later(codec), metadata = Metadata.Empty))

  trait Tagged[Self[_], -Value[_]]:
    def branch[A](name: String, codec: => Value[A]): Self[A]

    extension [A](self: Self[A])
      def discriminator: Discriminator
      def keyed: Self[A]
      def merged(discriminator: Discriminator.Merged): Self[A]
      final def merged: Self[A] = merged(discriminator = Discriminator.Merged.Default)
      def explicit(discriminator: Discriminator.Explicit): Self[A]
      final def explicit: Self[A] = explicit(discriminator = Discriminator.Explicit.Default)
