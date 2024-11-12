// package io.taig.otter

// import cats.data.NonEmptyChain
// import cats.syntax.all.*
// import io.taig.otter.Data.Required
// import cats.data.NonEmptyChainImpl.Type
// import io.taig.otter.Codec.Result

// sealed abstract class Sum[F[+_ <: Data] <: Data, +O <: Data, A] extends Codec[F[O], A]:
//   self =>

//   def branches: NonEmptyChain[Branch[?, ?]]

//   override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A]

//   override def modifyDefault(f: Option[A] => Option[A]): Sum[F, O, A]

//   override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]

//   override def to[B](using Convert[A, B]): Sum[F, O, B]

//   def orElse[P <: Data, B](codec: Sum[?, P, B]): Sum[F, O | P, Either[A, B]]

//   def :*[P <: Data, B](branch: Branch[P, B]): Sum[F, O | P, Either[A, B]]

//   def *:[P <: Data, B](branch: Branch[P, B]): Sum[F, P | O, Either[B, A]]

//   def nested(discriminator: Discriminator.Nested, default: Option[A], metadata: Metadata): Sum.Nested[O, A]

//   final def nested(discriminator: Discriminator.Nested): Sum.Nested[O, A] = nested(discriminator, default, metadata)

//   final def nested: Sum.Nested[O, A] = nested(Discriminator.Nested.Default)

//   def merged(discriminator: Discriminator.Merged): Sum.Merged[O, A]

//   final def merged: Sum.Merged[O, A] = merged(Discriminator.Merged.Default)

//   def keyed: Sum.Keyed[O, A]

//   def untagged: Sum.Untagged[O, A]

// object Sum:
//   sealed abstract class Nested[+O <: Data, A] extends Sum[[a <: Data] =>> Data.Object[Data.String | a], O, A]:
//     self =>

//     def discriminator: Discriminator.Nested

//     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[O, A] = new Nested[O, A]:
//       export self.{metadata, discriminator,branches, encode, decode}
//       override def default: Option[A] = f(self.default)
//       override def nested(discriminator: Discriminator.Nested, default: Option[A], metadata: Metadata): Sum.Nested[O, A] =
//         self.nested(discriminator, default, metadata)
//       override def merged(discriminator: Discriminator.Merged): Sum.Merged[O, A] = ???
//       override def keyed: Sum.Keyed[O, A] = ???
//       override def untagged: Sum.Untagged[O, A] = ???

//     final override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[O, A] = new Nested[O, A]:
//       export self.{branches, decode, default, discriminator, encode}
//       override def metadata: Metadata = f(self.metadata)
//       override def nested(discriminator: Discriminator.Nested, default: Option[A], metadata: Metadata): Sum.Nested[O, A] =
//         self.nested(discriminator, default, metadata)
//       override def merged(discriminator: Discriminator.Merged): Sum.Merged[O, A] = ???
//       override def keyed: Sum.Keyed[O, A] = self.keyed
//       override def untagged: Sum.Untagged[O, A] = ???

//     final override def imap[B](f: A => B)(g: B => A): Sum.Nested[O, B] = ???
//     final override def to[B](using convert: Convert[A, B]): Sum.Nested[O, B] = imap(convert.to)(convert.from)
//     final override def orElse[P <: Data, B](codec: Sum[?, P, B]): Sum.Nested[O | P, Either[A, B]] =
//       orElse(codec.nested)
//     final def orElse[P <: Data, B](codec: Sum.Nested[P, B]): Sum.Nested[O | P, Either[A, B]] =
//       new Nested[O | P, Either[A, B]]:
//         export self.{branches, discriminator, metadata}
//         override def default: Option[Either[A, B]] = none
//         override def nested(discriminator: Discriminator.Nested, default: Option[Either[A, B]], metadata: Metadata): Nested[O | P, Either[A, B]] =
//           self.nested(discriminator).orElse(codec.nested(discriminator))
//         override def merged(discriminator: Discriminator.Merged): Sum.Merged[O | P, Either[A, B]] =
//           self.merged(discriminator).orElse(codec.merged(discriminator))
//         override def keyed: Sum.Keyed[O | P, Either[A, B]] = self.keyed.orElse(codec.keyed)
//         override def untagged: Sum.Untagged[O | P, Either[A, B]] = self.untagged.orElse(codec.untagged)
//         override def decode(data: Data): Codec.Result[Either[A, B]] = ???
//         override def encode(ab: Either[A, B]): Data.Object[Data.String | O | P] =
//           ab.fold(self.encode, codec.encode)
//     final override def :*[P <: Data, B](branch: Branch[P, B]): Sum.Nested[O | P, Either[A, B]] =
//       orElse(Nested(branch))
//     final override def *:[P <: Data, B](branch: Branch[P, B]): Sum.Nested[P | O, Either[B, A]] =
//       Nested(branch).orElse(self)

//   object Nested:
//     final private case class Apply[O <: Data, A](
//         branch: Branch[O, A],
//         default: Option[A],
//         discriminator: Discriminator.Nested,
//         metadata: Metadata
//     ) extends Nested[O, A]:
//       override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//       override def nested(discriminator: Discriminator.Nested, default: Option[A], metadata: Metadata): Sum.Nested[O, A] = ???
//       override def merged(discriminator: Discriminator.Merged): Sum.Merged[O, A] = ???
//       override def keyed: Sum.Keyed[O, A] = ???
//       override def untagged: Sum.Untagged[O, A] = ???
//       override def decode(data: Data): Codec.Result[A] = ???
//       override def encode(a: A): Data.Object[Data.String | O] = ???

//     def apply[O <: Data, A](
//         branch: Branch[O, A],
//         default: Option[A],
//         discriminator: Discriminator.Nested,
//         metadata: Metadata
//     ): Sum.Nested[O, A] =
//       Apply(branch, default, discriminator, metadata)
//     def apply[O <: Data, A](branch: Branch[O, A]): Sum.Nested[O, A] =
//       Nested(branch, default = none, discriminator = Discriminator.Nested.Default, metadata = Metadata.Empty)

//   sealed abstract class Merged[+O <: Data, A] extends Sum[[a <: Data] =>> Data.Object[Data.String | a], O, A] {
//     final override def orElse[P <: Data, B](codec: Sum[?, P, B]): Sum.Merged[O | P, Either[A, B]] =
//       orElse(codec.merged)
//     final def orElse[P <: Data, B](codec: Sum.Merged[P, B]): Sum.Merged[O | P, Either[A, B]] = ???
//   }

//   sealed abstract class Keyed[+O <: Data, A] extends Sum[Data.Object, O, A]:
//     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[O, A] = ???
//     final override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[O, A] = ???
//     final override def imap[B](f: A => B)(g: B => A): Sum.Keyed[O, B] = ???
//     final override def to[B](using Convert[A, B]): Sum[Data.Object, O, B] = ???
//     final override def orElse[P <: Data, B](codec: Sum[?, P, B]): Sum.Keyed[O | P, Either[A, B]] =
//       orElse(codec.keyed)
//     final def orElse[P <: Data, B](codec: Sum.Keyed[P, B]): Sum.Keyed[O | P, Either[A, B]] = ???
//     override def :*[P <: Data, B](branch: Branch[P, B]): Sum.Keyed[O | P, Either[A, B]] = ???
//     override def *:[P <: Data, B](branch: Branch[P, B]): Sum.Keyed[P | O, Either[B, A]] = ???

//   object Keyed:
//     final private case class Apply[O <: Data, A](branch: Branch[O, A], default: Option[A], metadata: Metadata)
//         extends Sum.Keyed[O, A]:
//       override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//       override def nested(discriminator: Discriminator.Nested): Sum.Nested[O, A] = ???
//       override def merged(discriminator: Discriminator.Merged): Sum.Merged[O, A] = ???
//       override def keyed: Sum.Keyed[O, A] = this
//       override def untagged: Sum.Untagged[O, A] = Untagged(branch)
//       override def decode(data: Data): Codec.Result[A] = ???
//       override def encode(a: A): Data.Object[O] = ???

//     def apply[O <: Data, A](branch: Branch[O, A], default: Option[A], metadata: Metadata): Sum.Keyed[O, A] =
//       Apply(branch, default, metadata)
//     def apply[O <: Data, A](branch: Branch[O, A]): Sum.Keyed[O, A] =
//       Apply(branch, default = none, metadata = Metadata.Empty)

//   sealed abstract class Untagged[+O <: Data, A] extends Sum[Data.Required, O, A]:
//     self =>
//     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[O, A] = new Untagged[O, A]:
//       export self.{branches, decode, encode, metadata}
//       override def default: Option[A] = f(self.default)
//       override def nested(discriminator: Discriminator.Nested): Nested[O, A] = self.nested(discriminator)
//       override def merged(discriminator: Discriminator.Merged): Merged[O, A] = ???
//       override def keyed: Keyed[O, A] = ???
//       override def untagged: Untagged[O, A] = ???
//     final override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[O, A] = ???
//     final override def imap[B](f: A => B)(g: B => A): Sum.Untagged[O, B] = ???
//     final override def to[B](using Convert[A, B]): Sum.Untagged[O, B] = ???
//     final override def orElse[P <: Data, B](codec: Sum[?, P, B]): Sum.Untagged[O | P, Either[A, B]] =
//       orElse(codec.untagged)
//     final def orElse[P <: Data, B](codec: Sum.Untagged[P, B]): Sum.Untagged[O | P, Either[A, B]] =
//       new Untagged[O | P, Either[A, B]]:
//         override def branches: NonEmptyChain[Branch[?, ?]] = self.branches ++ codec.branches
//         override def default: Option[Either[A, B]] = none
//         override def metadata: Metadata = Metadata.Empty
//         override def merged(discriminator: Discriminator.Merged): Merged[O | P, Either[A, B]] = ???
//         override def nested(discriminator: Discriminator.Nested): Nested[O | P, Either[A, B]] = ???
//         override def keyed: Sum.Keyed[O | P, Either[A, B]] = self.keyed.orElse(codec.keyed)
//         override def untagged: Untagged[O | P, Either[A, B]] = this
//         override def decode(data: Data): Codec.Result[Either[A, B]] = ???
//         override def encode(ab: Either[A, B]): O | P = ab.fold(self.encode, codec.encode)

//     override def :*[P <: Data, B](branch: Branch[P, B]): Sum.Untagged[O | P, Either[A, B]] = ???
//     override def *:[P <: Data, B](branch: Branch[P, B]): Sum.Untagged[P | O, Either[B, A]] = ???

//   object Untagged:
//     final private case class Apply[O <: Data, A](branch: Branch[O, A], default: Option[A], metadata: Metadata)
//         extends Sum.Untagged[O, A] {
//       override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//       override def nested(discriminator: Discriminator.Nested): Sum.Nested[O, A] = ???
//       override def merged(discriminator: Discriminator.Merged): Sum.Merged[O, A] = ???
//       override def keyed: Sum.Keyed[O, A] = ???
//       override def untagged: Sum.Untagged[O, A] = ???
//       override def decode(data: Data): Codec.Result[A] = ???
//       override def encode(a: A): O = ???
//     }

//     def apply[O <: Data, A](branch: Branch[O, A]): Sum.Untagged[O, A] = ???

//   // def apply[O <: Data, A](branch: Branch[O, A]): Sum[Data.String, O, A] = new Sum[Data.String, O, A]:
//   //   override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//   //   override def metadata: Metadata = Metadata.Empty
//   //   override def default: Option[A] = none
//   //   override def decode(data: Data): Codec.Result[A] = nested.decode(data)
//   //   override def encode(a: A): Data.Object[Data.String | O] = ???
//   //   override def keyed: Sum[Nothing, O, A] = ???

// //   sealed abstract class Nested[+F[+a] <: Data.Nullable[a], +O <: Data, A]
// //       extends Sum[F, Data.Object[Data.String | O], A]:
// //     self =>

// //     final def discriminator: Attribute[Sum.Nested[F, O, A], Discriminator.Nested] =
// //       Attribute(this, Keys.discriminator.nested, Discriminator.Nested.Default)

// //     final override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = new Nested[F, O, A]:
// //       export self.{branches, decode, default, encode}
// //       override def metadata: Metadata = f(self.metadata)

// //     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[F, O, A] = new Nested[F, O, A]:
// //       export self.{branches, encode, metadata}
// //       override def default: Option[A] = f(self.default)
// //       override def decode(
// //           data: Option[Vector[(String, Data)]],
// //           discriminator: Discriminator.Nested
// //       ): Codec.Result[Option[A]] = data.fold(default.valid)(_ => self.decode(data, discriminator))

// //     final override def imap[B](f: A => B)(g: B => A): Sum.Nested[F, O, B] = new Sum.Nested[F, O, B]:
// //       export self.{branches, metadata}
// //       override def default: Option[B] = self.default.map(f)
// //       override def decode(
// //           data: Option[Vector[(String, Data)]],
// //           discriminator: Discriminator.Nested
// //       ): Codec.Result[Option[B]] = self.decode(data, discriminator).map(_.map(f))
// //       override def encode(b: B, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]] =
// //         self.encode(g(b), discriminator)

// //     final override def to[B](using convert: Convert[A, B]): Sum.Nested[F, O, B] = imap(convert.to)(convert.from)

// //     final def orElse[G[+a] >: F[a] <: Data.Nullable[a], P <: Data, B](
// //         codec: Sum.Nested[G, P, B]
// //     ): Sum.Nested[G, O | P, Either[A, B]] = new Nested[G, O | P, Either[A, B]]:
// //       override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
// //       override def metadata: Metadata = Metadata.Empty
// //       override def default: Option[Either[A, B]] = None
// //       override def decode(
// //           data: Option[Vector[(String, Data)]],
// //           discriminator: Discriminator.Nested
// //       ): Codec.Result[Option[Either[A, B]]] = self
// //         .decode(data, discriminator)
// //         .andThen:
// //           case Some(a) => a.asLeft.some.valid
// //           case None    => codec.decode(data, discriminator).map(_.map(_.asRight))
// //       override def encode(
// //           ab: Either[A, B],
// //           discriminator: Discriminator.Nested
// //       ): G[Data.Object[Data.String | (O | P)]] = ab.fold(self.encode(_, discriminator), codec.encode(_, discriminator))

// //     final override def decode(data: Data): Codec.Result[A] =
// //       val discriminator = self.discriminator.value

// //       data
// //         .match
// //           case Data.Object(values) => decode(values.some, discriminator)
// //           case Data.Null           => decode(none, discriminator)
// //           case _                   => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
// //         .andThen(
// //           _.toValid(
// //             Violations.namespaceNec(
// //               Data.RequiredPath.Root / discriminator.identifier,
// //               Violation(
// //                 Constraint.OneOf(branches.toNev.toList.map(branch => Data.String(branch.name))),
// //                 actual = data.asObject
// //                   .map(_.values)
// //                   .orEmpty
// //                   .collectFirst { case (name, data) if name === discriminator.identifier => data }
// //                   .getOrElse(Data.Null)
// //               )
// //             )
// //           )
// //         )

// //     def decode(data: Option[Vector[(String, Data)]], discriminator: Discriminator.Nested): Codec.Result[Option[A]]

// //     final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator.value)

// //     def encode(a: A, discriminator: Discriminator.Nested): F[Data.Object[Data.String | O]]

// //   object Nested:
// //     def apply[O <: Data, A](branches: => Branches[O, A]): Sum.Nested[Data.Required, O, A] =
// //       val _branches = branches

// //       new Nested[Data.Required, O, A]:
// //         override def branches: Branches[O, A] = _branches
// //         override def metadata: Metadata = Metadata.Empty
// //         override def default: Option[A] = None
// //         override def decode(
// //             data: Option[Vector[(String, Data)]],
// //             discriminator: Discriminator.Nested
// //         ): Codec.Result[Option[A]] = data
// //           .toValid(Violations.rootNec(Violation.tpe("object", actual = "null")))
// //           .andThen(branches.decodeNested(_, discriminator))
// //         override def encode(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | O] =
// //           branches.encodeNested(a, discriminator)

// //     extension [F[+a] <: Data.Nullable[a], O <: Data, A <: Matchable](self: Sum.Nested[F, O, A])
// //       inline def |[G[+a] >: F[a] <: Data.Nullable[a], P <: Data, B <: Matchable](
// //           codec: Sum.Nested[G, P, B]
// //       ): Sum.Nested[G, O | P, A | B] = self
// //         .orElse(codec)
// //         .imap {
// //           case Left(a)  => a
// //           case Right(b) => b
// //         } {
// //           case a: A => Left(a)
// //           case b: B => Right(b)
// //         }

// //     given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Sum.Nested[F, O, *]] with
// //       override def imap[A, B](fa: Sum.Nested[F, O, A])(f: A => B)(g: B => A): Sum.Nested[F, O, B] = fa.imap(f)(g)

// //     given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Sum.Nested[F, O, A]] with
// //       extension (self: Sum.Nested[F, O, A])
// //         override def metadata: Metadata = self.metadata
// //         override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = self.modifyMetadata(f)

// //   sealed abstract class Merged[+F[+a] <: Data.Nullable[a], +O <: Data, A]
// //       extends Sum[F, Data.Object[Data.String | O], A]:
// //     self =>

// //     def discriminator: Attribute[Sum.Merged[F, O, A], Discriminator.Merged] =
// //       Attribute(this, Keys.discriminator.merged, Discriminator.Merged.Default)

// //     final override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = new Merged[F, O, A]:
// //       export self.{branches, decode, default, encode}
// //       override def metadata: Metadata = f(self.metadata)

// //     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Merged[F, O, A] = ???

// //     final override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???

// //     final override def to[B](using convert: Convert[A, B]): Sum.Merged[F, O, B] = imap(convert.to)(convert.from)

// //     final override def decode(data: Data): Codec.Result[A] = data.asObject
// //       .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
// //       .map(_.values)
// //       .andThen(decode(_, discriminator.value))

// //     def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A]

// //     final override def encode(a: A): F[Data.Object[Data.String | O]] = encode(a, discriminator.value)

// //     def encode(a: A, discriminator: Discriminator.Merged): F[Data.Object[Data.String | O]]

// //   object Merged:
// //     def apply[O <: Data, A](branches: => Branches[Data.Object[O], A]): Sum.Merged[Data.Required, O, A] =
// //       val _branches = branches

// //       new Merged[Data.Required, O, A]:
// //         override def branches: Branches[Data.Object[O], A] = _branches
// //         override def metadata: Metadata = Metadata.Empty
// //         override def default: Option[A] = None
// //         override def decode(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[A] = ???
// //         override def encode(a: A, discriminator: Discriminator.Merged): Data.Object[Data.String | O] =
// //           branches.encodeMerged(a, discriminator)

// //     given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Sum.Merged[F, O, *]] with
// //       override def imap[A, B](fa: Sum.Merged[F, O, A])(f: A => B)(g: B => A): Sum.Merged[F, O, B] = fa.imap(f)(g)

// //     given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Sum.Merged[F, O, A]] with
// //       extension (self: Sum.Merged[F, O, A])
// //         override def metadata: Metadata = self.metadata
// //         override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = self.modifyMetadata(f)

// //   sealed abstract class Keyed[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]:
// //     self =>

// //     final override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = new Keyed[F, O, A]:
// //       export self.{branches, decode, default, encode}
// //       override def metadata: Metadata = f(self.metadata)

// //     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[F, O, A] = ???

// //     final override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = new Keyed[F, O, B]:
// //       export self.{branches, metadata}
// //       override def default: Option[B] = self.default.map(f)
// //       override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[Option[B]] =
// //         self.decode(data).map(_.map(f))
// //       override def encode(b: B): F[Data.Object[O]] = self.encode(g(b))

// //     final override def to[B](using convert: Convert[A, B]): Sum.Keyed[F, O, B] = imap(convert.to)(convert.from)

// //     final override def decode(data: Data): Codec.Result[A] = data
// //       .match
// //         case Data.Object(values) => decode(values.some)
// //         case Data.Null           => decode(none)
// //         case _                   => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
// //       .andThen(
// //         _.toValid(
// //           Violations.rootNec(
// //             Violation.oneOf(
// //               branches.toNev.toList.map(_.name),
// //               actual = data.asObject.map(_.values).flatMap(_.headOption).map { case (key, _) => key }.getOrElse("null")
// //             )
// //           )
// //         )
// //       )

// //     def decode(data: Option[Vector[(String, Data)]]): Codec.Result[Option[A]]

// //   object Keyed:
// //     def apply[O <: Data, A](branches: => Branches[O, A]): Sum.Keyed[Data.Required, O, A] =
// //       val _branches = branches

// //       new Keyed[Data.Required, O, A]:
// //         override def branches: Branches[O, A] = _branches
// //         override def metadata: Metadata = Metadata.Empty
// //         override def default: Option[A] = None
// //         override def decode(data: Option[Vector[(String, Data)]]): Codec.Result[Option[A]] =
// //           data.toValid(Violations.rootNec(Violation.tpe("object", "null"))).andThen(branches.decodeKeyed)
// //         override def encode(a: A): Data.Object[O] = branches.encodeKeyed(a)

// //     given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Sum.Keyed[F, O, *]] with
// //       override def imap[A, B](fa: Sum.Keyed[F, O, A])(f: A => B)(g: B => A): Sum.Keyed[F, O, B] = fa.imap(f)(g)

// //     given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Sum.Keyed[F, O, A]] with
// //       extension (self: Sum.Keyed[F, O, A])
// //         override def metadata: Metadata = self.metadata
// //         override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = self.modifyMetadata(f)

// //   sealed abstract class Untagged[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Sum[F, O, A]:
// //     self =>

// //     final override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = new Untagged[F, O, A]:
// //       export self.{branches, decode, default, encode}
// //       override def metadata: Metadata = f(self.metadata)

// //     final override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???

// //     final override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = new Sum.Untagged[F, O, B]:
// //       export self.{branches, metadata}
// //       override def default: Option[B] = self.default.map(f)
// //       override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
// //       override def encode(b: B): F[O] = self.encode(g(b))

// //     final override def to[B](using convert: Convert[A, B]): Sum.Untagged[F, O, B] = imap(convert.to)(convert.from)

// //     final def orElse[G[+a] >: F[a] <: Data.Nullable[a], P <: Data, B](
// //         codec: Sum.Untagged[G, P, B]
// //     ): Sum.Untagged[G, O | P, Either[A, B]] = new Untagged[G, O | P, Either[A, B]]:
// //       override def branches: Branches[?, ?] = self.branches.orElse(codec.branches)
// //       override def metadata: Metadata = Metadata.Empty
// //       override def default: Option[Either[A, B]] = None
// //       override def decode(data: Data): Codec.Result[Either[A, B]] = ???
// //       override def encode(ab: Either[A, B]): G[O | P] = ab.fold(self.encode, codec.encode)

// //   object Untagged:
// //     def apply[O <: Data, A](branches: => Branches[O, A]): Sum.Untagged[Data.Required, O, A] =
// //       val _branches = branches

// //       new Untagged[Data.Required, O, A]:
// //         override def branches: Branches[O, A] = _branches
// //         override def metadata: Metadata = Metadata.Empty
// //         override def default: Option[A] = None
// //         override def decode(data: Data): Codec.Result[A] = branches.decodeUntagged(data)
// //         override def encode(a: A): O = branches.encodeUntagged(a)

// //     given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Sum.Untagged[F, O, *]] with
// //       override def imap[A, B](fa: Sum.Untagged[F, O, A])(f: A => B)(g: B => A): Sum.Untagged[F, O, B] = fa.imap(f)(g)

// //     given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Sum.Untagged[F, O, A]] with
// //       extension (self: Sum.Untagged[F, O, A])
// //         override def metadata: Metadata = self.metadata
// //         override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = self.modifyMetadata(f)

// //   given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Sum[F, O, *]] with
// //     override def imap[A, B](fa: Sum[F, O, A])(f: A => B)(g: B => A): Sum[F, O, B] = fa.imap(f)(g)

// //   given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Sum[F, O, A]] with
// //     extension (self: Sum[F, O, A])
// //       override def metadata: Metadata = self.metadata
// //       override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A] = self.modifyMetadata(f)
