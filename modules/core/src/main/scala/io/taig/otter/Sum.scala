package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.Data.Optional

sealed abstract class Sum[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, O, A]:
  def branches: Branches[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Sum[F, O, A]
  override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]
  override def to[B](using convert: Convert[A, B]): Sum[F, O, B]
  override def optional: Sum[Data.Optional, O, Option[A]]

object Sum:
  sealed abstract class Nested[+F[+a] <: Data.Optional[a], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A]:
    self =>

    final def discriminator: Attribute[Sum.Nested[F, O, A], Discriminator.Nested] =
      Attribute(this, Keys.discriminator.nested, Discriminator.Nested.Default)

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = new Nested[F, O, A]:
      export self.{branches, decode, default, encode, isOptional}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[F, O, A] = new Nested[F, O, A]:
      export self.{branches, encode, metadata}
      override def default: Option[A] = f(self.default)
      override def isOptional: Boolean = default.nonEmpty
      override def decode(
          data: Option[Vector[(String, Data)]],
          discriminator: Discriminator.Nested
      ): Codec.Result[Option[A]] = data.fold(default.valid)(_ => self.decode(data, discriminator))

    final override def imap[B](f: A => B)(g: B => A): Sum.Nested[F, O, B] = new Sum.Nested[F, O, B]:
      export self.{branches, isOptional, metadata}
      override def default: Option[B] = self.default.map(f)
      override def decode(
          data: Option[Vector[(String, Data)]],
          discriminator: Discriminator.Nested
      ): Codec.Result[Option[B]] =
        self.decode(data, discriminator).map(_.map(f))
      override def encode(b: B, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]] =
        self.encode(g(b), discriminator)

    final override def to[B](using convert: Convert[A, B]): Sum.Nested[F, O, B] = imap(convert.to)(convert.from)

    final override def optional: Sum.Nested[Data.Optional, O, Option[A]] = new Nested[Data.Optional, O, Option[A]]:
      export self.{branches, metadata}
      override def isOptional: Boolean = true
      override def default: Option[Option[A]] = self.default.map(_.some)
      override def decode(
          data: Option[Vector[(String, Data)]],
          discriminator: Discriminator.Nested
      ): Codec.Result[Option[Option[A]]] = data.fold(default.valid)(_ => self.decode(data, discriminator).map(_.some))
      override def encode(
          a: Option[A],
          discriminator: Discriminator.Nested
      ): Data.Optional[Data.Object[Data.String | O]] = a.fold(Data.Null)(self.encode)

    final def orElse[G[+a] >: F[a] <: Data.Optional[a], P <: Data, B](
        codec: Sum.Nested[G, P, B]
    ): Sum.Nested[G, O | P, Either[A, B]] = new Nested[G, O | P, Either[A, B]]:
      override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
      override def isOptional: Boolean = false
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(
          data: Option[Vector[(String, Data)]],
          discriminator: Discriminator.Nested
      ): Codec.Result[Option[Either[A, B]]] = self
        .decode(data, discriminator)
        .andThen:
          case Some(a) => a.asLeft.some.valid
          case None    => codec.decode(data, discriminator).map(_.map(_.asRight))
      override def encode(
          ab: Either[A, B],
          discriminator: Discriminator.Nested
      ): G[Data.Object[Data.String | (O | P)]] = ab.fold(self.encode(_, discriminator), codec.encode(_, discriminator))

    final override def decode(data: Data): Codec.Result[A] =
      val discriminator = self.discriminator.value

      data
        .match
          case Data.Object(values) => decode(values.some, discriminator)
          case Data.Null           => decode(none, discriminator)
          case _                   => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
        .andThen(
          _.toValid(
            Violations.namespaceNec(
              XPath.Root / discriminator.identifier,
              Violation(
                Constraint.OneOf(branches.toNev.toNonEmptyList.map(branch => Data.String(branch.name))),
                actual = data.asObject
                  .map(_.values)
                  .orEmpty
                  .collectFirst { case (name, data) if name === discriminator.identifier => data }
                  .getOrElse(Data.Null)
              )
            )
          )
        )

    def decode(data: Option[Vector[(String, Data)]], discriminator: Discriminator.Nested): Codec.Result[Option[A]]

    final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator.value)

    def encode(a: A, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]]

  object Nested:
    def apply[O <: Data, A](branches: => Branches[O, A]): Sum.Nested[Data.Required, O, A] =
      val _branches = branches

      new Nested[Data.Required, O, A]:
        override def branches: Branches[O, A] = _branches
        override def isOptional: Boolean = false
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(
            data: Option[Vector[(String, Data)]],
            discriminator: Discriminator.Nested
        ): Codec.Result[Option[A]] =
          data
            .toValid(Violations.rootNec(Violation.tpe("object", actual = "null")))
            .andThen(branches.decodeNested(_, discriminator))
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

    given [F[+a] <: Data.Optional[a], O <: Data]: CodecInvariant[Sum.Nested[F, O, *]] with
      override def imap[A, B](fa: Sum.Nested[F, O, A])(f: A => B)(g: B => A): Sum.Nested[F, O, B] = fa.imap(f)(g)

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
      export self.{branches, decode, default, encode, isOptional}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Merged[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???

    final override def to[B](using convert: Convert[A, B]): Sum.Merged[F, O, B] = imap(convert.to)(convert.from)

    final override def optional: Sum.Merged[Data.Optional, O, Option[A]] = ???

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode(_, discriminator.value))

    def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A]

    final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator.value)

    def encode(a: A, discriminator: Discriminator.Merged): F[Data.Object[Data.String | O]]

  object Merged:
    def apply[O <: Data, A](branches: => Branches[Data.Object[O], A]): Sum.Merged[Data.Required, O, A] =
      val _branches = branches

      new Merged[Data.Required, O, A]:
        override def branches: Branches[Data.Object[O], A] = _branches
        override def isOptional: Boolean = false
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A] = ???
        override def encode(a: A, discriminator: Discriminator.Merged): Data.Object[Data.String | O] =
          branches.encodeMerged(a, discriminator)

    given [F[+a] <: Data.Optional[a], O <: Data]: CodecInvariant[Sum.Merged[F, O, *]] with
      override def imap[A, B](fa: Sum.Merged[F, O, A])(f: A => B)(g: B => A): Sum.Merged[F, O, B] = fa.imap(f)(g)

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Merged[F, O, A]] with
      extension (self: Sum.Merged[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = self.modifyMetadata(f)

  sealed abstract class Keyed[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = new Keyed[F, O, A]:
      export self.{branches, decode, default, encode, isOptional}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = new Keyed[F, O, B]:
      export self.{branches, isOptional, metadata}
      override def default: Option[B] = self.default.map(f)
      override def decode(data: Vector[(String, Data)]): Codec.Result[B] = self.decode(data).map(f)
      override def encode(b: B): F[Data.Object[O]] = self.encode(g(b))

    final override def to[B](using convert: Convert[A, B]): Sum.Keyed[F, O, B] = imap(convert.to)(convert.from)

    final override def optional: Sum.Keyed[Data.Optional, O, Option[A]] = ???

    final override def decode(data: Data): Codec.Result[A] = data.asObject
      .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
      .map(_.values)
      .andThen(decode)

    def decode(data: Vector[(String, Data)]): Codec.Result[A]

  object Keyed:
    def apply[O <: Data, A](branches: => Branches[O, A]): Sum.Keyed[Data.Required, O, A] =
      val _branches = branches

      new Keyed[Data.Required, O, A]:
        override def branches: Branches[O, A] = _branches
        override def isOptional: Boolean = false
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Vector[(String, Data)]): Codec.Result[A] =
          branches.decodeKeyed(data)
          ???
        override def encode(a: A): Data.Object[O] = branches.encodeKeyed(a)

    given [F[+a] <: Data.Optional[a], O <: Data]: CodecInvariant[Sum.Keyed[F, O, *]] with
      override def imap[A, B](fa: Sum.Keyed[F, O, A])(f: A => B)(g: B => A): Sum.Keyed[F, O, B] = fa.imap(f)(g)

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Keyed[F, O, A]] with
      extension (self: Sum.Keyed[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = self.modifyMetadata(f)

  sealed abstract class Untagged[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, O, A]:
    self =>

    final override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = new Untagged[F, O, A]:
      export self.{branches, decode, default, encode, isOptional}
      override def metadata: Metadata = f(self.metadata)

    final override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???

    final override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = new Sum.Untagged[F, O, B]:
      export self.{branches, isOptional, metadata}
      override def default: Option[B] = self.default.map(f)
      override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
      override def encode(b: B): F[O] = self.encode(g(b))

    final override def to[B](using convert: Convert[A, B]): Sum.Untagged[F, O, B] = imap(convert.to)(convert.from)

    final override def optional: Sum.Untagged[Data.Optional, O, Option[A]] = ???

    final def orElse[G[+a] >: F[a] <: Data.Optional[a], P <: Data, B](
        codec: Sum.Untagged[G, P, B]
    ): Sum.Untagged[G, O | P, Either[A, B]] = new Untagged[G, O | P, Either[A, B]]:
      override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
      override def isOptional: Boolean = false
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Either[A, B]] = None
      override def decode(data: Data): Codec.Result[Either[A, B]] = ???
      override def encode(ab: Either[A, B]): G[O | P] = ab.fold(self.encode, codec.encode)

  object Untagged:
    def apply[O <: Data, A](branches: => Branches[O, A]): Sum.Untagged[Data.Required, O, A] =
      val _branches = branches

      new Untagged[Data.Required, O, A]:
        override def branches: Branches[O, A] = _branches
        override def isOptional: Boolean = false
        override def metadata: Metadata = Metadata.Empty
        override def default: Option[A] = None
        override def decode(data: Data): Codec.Result[A] = branches.decodeUntagged(data)
        override def encode(a: A): O = branches.encodeUntagged(a)

    given [F[+a] <: Data.Optional[a], O <: Data]: CodecInvariant[Sum.Untagged[F, O, *]] with
      override def imap[A, B](fa: Sum.Untagged[F, O, A])(f: A => B)(g: B => A): Sum.Untagged[F, O, B] = fa.imap(f)(g)

    given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum.Untagged[F, O, A]] with
      extension (self: Sum.Untagged[F, O, A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = self.modifyMetadata(f)

  given [F[+a] <: Data.Optional[a], O <: Data]: CodecInvariant[Sum[F, O, *]] with
    override def imap[A, B](fa: Sum[F, O, A])(f: A => B)(g: B => A): Sum[F, O, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Sum[F, O, A]] with
    extension (self: Sum[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A] = self.modifyMetadata(f)
