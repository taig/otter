package io.taig.otter

import cats.syntax.all.*
import cats.Id as Identity
import io.taig.otter.Data.Optional

sealed abstract class Sum[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, O, A]:
  def branches: Branches[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Sum[F, O, A]
  override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]
  def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Sum[F, O, B]
  override def optional: Sum[Data.Optional, O, Option[A]]

object Sum:
  sealed abstract class Nested[+F[+a] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    self =>

    final def discriminator: Attribute[Sum.Nested[F, O, A], Discriminator.Nested] =
      Attribute(this, Keys.discriminator.nested, Discriminator.Nested.Default)

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = new Nested[F, O, A]:
      export self.{branches, decode, default, encode}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Nested[F, O, B] = new Sum.Nested[F, O, B]:
      export self.{branches, metadata}
      override def default: Option[B] = self.default.map(f)
      override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Nested): Codec.Result[B] =
        self.decode(data, discriminator).map(f)
      override def encode(b: B, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]] =
        self.encode(g(b), discriminator)

    final override def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Sum.Nested[F, O, B] =
      imap(evidence.from)(evidence.to)

    final override def optional: Sum.Nested[Data.Optional, O, Option[A]] = ???

    final def orElse[G[+a] >: F[a] <: Data.Optional[a], P <: Data, B](
        codec: Sum.Nested[G, P, B]
    ): Sum.Nested[G, O | P, Either[A, B]] = new Nested[G, O | P, Either[A, B]]:
      override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(
          data: Vector[(String, Data)],
          discriminator: Discriminator.Nested
      ): Codec.Result[Either[A, B]] = ???
      override def encode(
          ab: Either[A, B],
          discriminator: Discriminator.Nested
      ): G[Data.Object[Data.String | (O | P)]] = ab.fold(self.encode(_, discriminator), codec.encode(_, discriminator))

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode(_, discriminator.value))

    def decode(data: Vector[(String, Data)], discriminator: Discriminator.Nested): Codec.Result[A]

    final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator.value)

    def encode(a: A, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]]

  object Nested:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Nested[Data.Required, O, A] =
      val _branches = branches

      new Nested[Data.Required, O, A]:
        override def branches: Branches[O, A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Nested): Codec.Result[A] =
          branches
            .decodeNested(data, discriminator)
            .andThen(
              _.toValid(
                Violations.namespaceNec(
                  Step.Field(discriminator.identifier),
                  Violation(
                    Constraint.Primitive.OneOf(branches.toNev.toList.map(branch => Data.String(branch.name))),
                    actual = data
                      .collectFirst { case (name, data) if name === discriminator.identifier => data }
                      .getOrElse(Data.Null)
                  )
                )
              )
            )
        override def encode(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | O] =
          branches.encodeNested(a, discriminator)

    extension [F[+a] <: Data.Optional[a], O <: Data, A <: Matchable](self: Sum.Nested[F, O, A])
      inline def |[G[+a] >: F[a] <: Data.Optional[a], P <: Data, B <: Matchable](
          codec: Sum.Nested[G, P, B]
      ): Sum.Nested[G, O | P, A | B] = self
        .orElse(codec)
        .imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Nested[F, O, A]] with
      extension (self: Sum.Nested[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = self.modifyMetadata(f)

  sealed abstract class Merged[+F[+a] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    self =>

    def discriminator: Attribute[Sum.Merged[F, O, A], Discriminator.Merged] =
      Attribute(this, Keys.discriminator.merged, Discriminator.Merged.Default)

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = new Merged[F, O, A]:
      export self.{branches, decode, default, encode}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Merged[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???

    final override def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Sum.Merged[F, O, B] =
      imap(evidence.from)(evidence.to)

    final override def optional: Sum.Merged[Data.Optional, O, Option[A]] = ???

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode(_, discriminator.value))

    def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A]

    final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator.value)

    def encode(a: A, discriminator: Discriminator.Merged): F[Data.Object[Data.String | O]]

  object Merged:
    def apply[O <: Data, A](branches: Branches[Data.Object[O], A]): Sum.Merged[Data.Required, O, A] =
      val _branches = branches

      new Merged[Data.Required, O, A]:
        override def branches: Branches[Data.Object[O], A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A] = ???
        override def encode(a: A, discriminator: Discriminator.Merged): Data.Object[Data.String | O] =
          branches.encodeMerged(a, discriminator)

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Merged[F, O, A]] with
      extension (self: Sum.Merged[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = self.modifyMetadata(f)

  sealed abstract class Keyed[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = new Keyed[F, O, A]:
      export self.{branches, decode, default, encode}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = ???

    final override def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Sum.Keyed[F, O, B] =
      imap(evidence.from)(evidence.to)

    final override def optional: Sum.Keyed[Data.Optional, O, Option[A]] = ???

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode)

    def decode(data: Vector[(String, Data)]): Codec.Result[A]

  object Keyed:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Keyed[Data.Required, O, A] =
      val _branches = branches

      new Keyed[Data.Required, O, A]:
        override def branches: Branches[O, A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)]): Codec.Result[A] =
          branches.decodeKeyed(data)
          ???
        override def encode(a: A): Data.Object[O] = branches.encodeKeyed(a)

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Keyed[F, O, A]] with
      extension (self: Sum.Keyed[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = self.modifyMetadata(f)

  sealed abstract class Untagged[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, O, A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = new Untagged[F, O, A]:
      export self.{branches, decode, default, encode}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = new Sum.Untagged[F, O, B]:
      export self.{branches, metadata}
      override def default: Option[B] = self.default.map(f)
      override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
      override def encode(b: B): F[O] = self.encode(g(b))

    final override def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Sum.Untagged[F, O, B] =
      imap(evidence.from)(evidence.to)

    final override def optional: Sum.Untagged[Optional, O, Option[A]] = ???

    final def orElse[G[+a] >: F[a] <: Data.Optional[a], P <: Data, B](
        codec: Sum.Untagged[G, P, B]
    ): Sum.Untagged[G, O | P, Either[A, B]] = new Untagged[G, O | P, Either[A, B]]:
      override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(data: Data): Codec.Result[Either[A, B]] = ???
      override def encode(ab: Either[A, B]): G[O | P] = ab.fold(self.encode, codec.encode)

  object Untagged:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Untagged[Data.Required, O, A] =
      val _branches = branches

      new Untagged[Data.Required, O, A]:
        override def branches: Branches[O, A] = _branches
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Data): Codec.Result[A] = branches.decodeUntagged(data)
        override def encode(a: A): O = branches.encodeUntagged(a)

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Untagged[F, O, A]] with
      extension (self: Sum.Untagged[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = self.modifyMetadata(f)

  given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum[F, O, A]] with
    extension (self: Sum[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A] = self.modifyMetadata(f)
