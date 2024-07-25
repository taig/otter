// package io.taig.otter

// import cats.data.NonEmptyChain
// import cats.syntax.all.*
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import cats.data.Chain
// import cats.Invariant
// import io.taig.otter.Data.Optional
// import io.taig.otter.Data.Value
// import io.taig.otter.Codec.Result
// import cats.data.NonEmptyChainImpl
// import cats.Id

// sealed abstract class Sum[+F[+_], G[+_ <: Data], +O <: Data, A] extends Codec[F, G[O], A]:
//   self =>

//   def branches: NonEmptyChain[Branch[?, ?]]

//   // def modifyDiscriminator[Q <: Data](f: Discriminator2[P] => Discriminator2[Q]): Sum[O, Q, A] = ???

//   override def modifyMetadata(f: Metadata => Metadata): Sum[F, G, O, A] = ???

//   override def modifyDefault(f: Option[A] => Option[A]): Sum[F, G, O, A] = ???

//   // def untagged: Sum[F, Data, A]

//   // final override def modifyMetadata(f: Metadata => Metadata): Sum[O, A] = new Sum[O, A]:
//   //   export self.{branches, decode, default, encode}
//   //   override def metadata: Metadata = f(self.metadata)

//   // final override def modifyDefault(f: Option[A] => Option[A]): Sum[O, A] = new Sum[O, A]:
//   //   export self.{branches, encode, metadata}
//   //   override def default: Option[A] = f(self.default)
//   //   override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[Option[A]] =
//   //     (data, default) match
//   //       case (None, Some(default)) => default.some.valid
//   //       case _                     => self.decode(data)

//   override def imap[B](f: A => B)(g: B => A): Sum[F, G, O, B] = ???
//   // final override def imap[B](f: A => B)(g: B => A): Sum[O, B] = new Sum[O, B]:
//   //   export self.{branches, metadata}
//   //   override def default: Option[B] = self.default.map(f)
//   //   override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[Option[B]] =
//   //     self.decode(data).map(_.map(f))
//   //   override def encode(b: B): O = self.encode(g(b))

//   // new Sum[O | P, Either[A, B]]:
//   //   override def branches: NonEmptyChain[Branch[?, ?]] = self.branches ++ sum.branches
//   //   override def metadata: Metadata = Metadata.Empty
//   //   override def default: Option[Either[A, B]] = None
//   //   override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[Option[Either[A, B]]] =
//   //     self
//   //       .decode(data)
//   //       .andThen:
//   //         case Some(a) => a.asLeft.some.valid
//   //         case None    => sum.decode(data).map(_.map(_.asRight))
//   //   override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, sum.encode)

//   // final override def optional: Sum[Data.Optional[O], Option[A]] = new Sum[Data.Optional[O], Option[A]]:
//   //   export self.{branches, metadata}
//   //   override def default: Option[Option[A]] = self.default.map(_.some)
//   //   override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[Option[Option[A]]] =
//   //     // TODO not sure if `default.valid` is correct
//   //     data.fold(default.valid)(_ => self.decode(data).map(_.some))
//   //   override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

//   final override def decode(data: Data): Codec.Result[A] = data
//     .match
//       case Data.Null           => decode(none)
//       case Data.Object(values) => decode(values.some)
//       case data => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid
//     .andThen(
//       _.toValid(
//         Violations.rootNec(
//           Violation(
//             Constraint.OneOf(branches.map(branch => Data.String(branch.name)).toList),
//             actual = Data.String("null")
//           )
//         )
//       )
//     )

//   def decode(data: Option[Chain[(String, Data)]]): Codec.Result[Option[A]]

// object Sum

// // sealed abstract class Merged[+F[+a <: Data.Value] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[Data.String | O], A]:
// //   override def merged[O1 <: Data](identifier: String)(using Data.Object[Data.String | O] <:< Data.Object[O1]): Sum[F, Data.Object[O1 | Data.String], A] = ???

// // sealed abstract class Keyed[+F[+a <: Data.Value] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]

// // sealed abstract class Flattened[+F[+a <: Data.Value] <: Data.Optional[a], +O <: Data, A] extends Sum[F, O, A]:
// //   override def flattened: Sum.Flattened[F, O, A] = this

// // def apply[O <: Data, A](branch: Branch[O, A]): Sum[Data.Object[?], A] = new Sum[Data.Object[?], A]:
// //   override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
// //   override def metadata: Metadata = Metadata.Empty
// //   override def default: Option[A] = None
// //   override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[Option[A]] = data
// //     .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))))
// //     .andThen(branch.decode)
// //   override def encode(a: A): Data.Object[?] = branch.encodeKeyed(a)

// // given [F[+a <: Data] <: Data.Optional[a], O <: Data]: Invariant[Sum[F, O, *]] with
// //   override def imap[A, B](fa: Sum[F, O, A])(f: A => B)(g: B => A): Sum[F, O, B] = fa.imap(f)(g)
