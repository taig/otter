package io.taig.otter

import cats.syntax.all.*
import cats.Id as Identity
import io.taig.otter.Data.Optional
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.History

sealed abstract class Sum[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A] extends Codec[F, O, A]:
  def branches: Branches[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Sum[F, O, A]
  override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]
  override def optional: Sum[Data.Optional, O, Option[A]]

object Sum:
  sealed abstract class Nested[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    self =>

    def discriminator: Discriminator.Nested

    final def modifyDiscriminator(f: Discriminator.Nested => Discriminator.Nested): Sum.Nested[F, O, A] =
      new Sum.Nested[F, O, A]:
        export self.{branches, decode, default, encode, metadata}
        override def discriminator: Discriminator.Nested = f(self.discriminator)

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = ???

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Nested[F, O, B] = ???

    final override def optional: Sum.Nested[Data.Optional, O, Option[A]] = ???

    final def orElse[G[+a <: Data] >: F[a] <: Data.Optional[a], P <: Data, B](
        codec: Sum.Nested[G, P, B]
    ): Sum.Nested[G, O | P, Either[A, B]] = new Nested[G, O | P, Either[A, B]]:
      override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
      override def discriminator: Discriminator.Nested = Discriminator.Nested.Default
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(
          data: Vector[(String, Data)],
          discriminator: Discriminator.Nested
      ): Codec.Result[Either[A, B]] = ???
      override def encode(
          ab: Either[A, B],
          discriminator: Discriminator.Nested
      ): G[Data.Object[Data.String | (O | P)]] =
        ab.fold(self.encode(_, discriminator), codec.encode(_, discriminator))

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode(_, discriminator))

    def decode(data: Vector[(String, Data)], discriminator: Discriminator.Nested): Codec.Result[A]

    final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator)

    def encode(a: A, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]]

  object Nested:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Nested[Identity, O, A] =
      val _branches = branches

      new Nested[Identity, O, A]:
        override def branches: Branches[O, A] = _branches
        override def discriminator: Discriminator.Nested = Discriminator.Nested.Default
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Nested): Codec.Result[A] =
          branches
            .decodeNested(data, discriminator)
            .andThen(
              _.toValid(
                Violations.namespaceNec(
                  History.Step.Field(discriminator.identifier),
                  Violation(
                    Constraint.OneOf(branches.toNev.toList.map(branch => Data.String(branch.name))),
                    actual = data
                      .collectFirst { case (name, data) if name === discriminator.identifier => data }
                      .getOrElse(Data.Null)
                  )
                )
              )
            )
        override def encode(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | O] =
          branches.encodeNested(a, discriminator)

  sealed abstract class Merged[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    self =>

    def discriminator: Discriminator.Merged

    final def modifyDiscriminator(f: Discriminator.Merged => Discriminator.Merged): Sum.Merged[F, O, A] =
      new Sum.Merged[F, O, A]:
        export self.{branches, decode, default, encode, metadata}
        override def discriminator: Discriminator.Merged = f(self.discriminator)

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = ???

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Merged[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???

    final override def optional: Sum.Merged[Data.Optional, O, Option[A]] = ???

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode(_, discriminator))

    def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A]

    final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator)

    def encode(a: A, discriminator: Discriminator.Merged): F[Data.Object[Data.String | O]]

  object Merged:
    def apply[O <: Data, A](branches: Branches[Data.Object[O], A]): Sum.Merged[Identity, O, A] =
      val _branches = branches

      new Merged[Identity, O, A]:
        override def branches: Branches[Data.Object[O], A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def discriminator: Discriminator.Merged = Discriminator.Merged.Default
        override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A] = ???
        override def encode(a: A, discriminator: Discriminator.Merged): Data.Object[Data.String | O] =
          branches.encodeMerged(a, discriminator)

  sealed abstract class Keyed[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = ???

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = ???

    final override def optional: Sum.Keyed[Data.Optional, O, Option[A]] = ???

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode)

    def decode(data: Vector[(String, Data)]): Codec.Result[A]

  object Keyed:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Keyed[Identity, O, A] =
      val _branches = branches

      new Keyed[Identity, O, A]:
        override def branches: Branches[O, A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)]): Codec.Result[A] =
          branches.decodeKeyed(data)
          ???
        override def encode(a: A): Data.Object[O] = branches.encodeKeyed(a)

  sealed abstract class Untagged[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A] extends Sum[F, O, A]:
    self =>
    final override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = ???

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = ???

    final override def optional: Sum.Untagged[Optional, O, Option[A]] = ???

    final def orElse[G[+a <: Data] >: F[a] <: Data.Optional[a], P <: Data, B](
        codec: Sum.Untagged[G, P, B]
    ): Sum.Untagged[G, O | P, Either[A, B]] = new Untagged[G, O | P, Either[A, B]]:
      override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(data: Data): Codec.Result[Either[A, B]] = ???
      override def encode(ab: Either[A, B]): G[O | P] = ab.fold(self.encode, codec.encode)

  object Untagged:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Untagged[Identity, O, A] =
      val _branches = branches

      new Untagged[Identity, O, A]:
        override def branches: Branches[O, A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Data): Codec.Result[A] = branches.decodeUntagged(data).andThen(???)
        override def encode(a: A): O = branches.encodeUntagged(a)
