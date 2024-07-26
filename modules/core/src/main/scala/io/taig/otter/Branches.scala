// package io.taig.otter

// sealed abstract class Branches[+F[+_], +O, A]:
//   self =>

//   final def orElse[G[+a] >: F[a], P, B](branches: Branches[G, P, B]): Branches[G, O | P, Either[A, B]] =
//     new Branches[G, O | P, Either[A, B]]:
//       override def encodeMerged[Q](a: Either[A, B], discriminator: Discriminator.Merged)(using (O | P) <:< Data.Object[Q]): Data.Object[Data.String | Q] = ???

//       override def encodeUntagged(ab: Either[A, B]): G[O | P] = ab.fold(self.encodeUntagged, branches.encodeUntagged)

//       override def decodeUntagged(data: Data): Codec.Result[Option[Either[A, B]]] = ???

//       override def encodeKeyed(a: Either[A, B]): Data.Object[G[O | P]] = ???

//       override def encodeNested(a: Either[A, B], discriminator: Discriminator.Nested): Data.Object[Data.String | G[O | P]] = ???

//   def decodeUntagged(data: Data): Codec.Result[Option[A]]

//   def encodeNested(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | F[O]]

//   def encodeMerged[P](a: A, discriminator: Discriminator.Merged)(using O <:< Data.Object[P]): Data.Object[Data.String | P]

//   def encodeKeyed(a: A): Data.Object[F[O]]

//   def encodeUntagged(a: A): F[O]

// object Branches:
//   def apply[F[+a] <: Data.Optional[a], O, A](branch: Branch[F, O, A]): Branches[F, O, A] = new Branches[F, O, A]:
//     override def decodeUntagged(data: Data): Codec.Result[Option[A]] = ???
//     override def encodeNested(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | F[O]] = ???
//     override def encodeMerged[P](a: A, discriminator: Discriminator.Merged)(using O <:< Data.Object[P]): Data.Object[Data.String | P] =
//       val result = Data.Object.one(discriminator.identifier, Data.String(branch.name))
//       branch.encode(a).fold(result)(_ ++ result)
//     override def encodeKeyed(a: A): Data.Object[F[O]] = Data.Object.one(branch.name, branch.encode(a))
//     override def encodeUntagged(a: A): F[O] = branch.encode(a)
