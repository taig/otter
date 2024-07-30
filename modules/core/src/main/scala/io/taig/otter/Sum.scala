package io.taig.otter

import cats.syntax.all.*
import cats.Id as Identity
import io.taig.otter.Codec.Result
import io.taig.otter.Data.Optional

sealed abstract class Sum[+F[+a <: Data] <: Data.Optional[a], +O <: Data.Value, A] extends Codec[F, O, A]:
  def branches: Branches[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Sum[F, O, A]
  override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]
  override def optional: Sum[Data.Optional, O, Option[A]]

object Sum:
  sealed abstract class Untagged[+F[+a <: Data] <: Data.Optional[a], +O <: Data.Value, A] extends Sum[F, O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = ???
    override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = ???
    override def optional: Sum.Untagged[Optional, O, Option[A]] = ???

  object Untagged:
    def apply[O <: Data, A](branches: Branches[O, A]): Sum.Untagged[Identity, O, A] = ???

// object Sum:
//   sealed abstract class Nested[+F[+_], +O, A] extends Sum[F, [a] =>> Data.Object[Data.String | a], O, A]:
//     self =>
//     override def orElse[H[+a] >: F[a], P, B](sum: Sum[H, ?, P, B]): Sum.Nested[H, O | P, Either[A, B]] = new Nested[H, O | P, Either[A, B]] {
//       override def branches: Branches[?, ?, ?] = self.branches.orElse(sum.branches)
//       override def metadata: Metadata = Metadata.Empty
//       override def default: Option[Either[A, B]] = None
//       override def decode(data: Data): Result[Either[A, B]] = ???
//       override def encode(ab: Either[A, B]): H[Data.Object[Data.String | (O | P)]] = ab.fold(self.encode, sum.nested(???).encode)
//     }

//     override def nested(discriminator: Discriminator.Nested): Sum.Nested[F, O, A] = this

//   object Nested:
//     def apply[F[+_], O, A](branches: Branches[F, O, A], discriminator: Discriminator.Nested): Sum.Nested[Identity, F[O], A] =
//       val _branches = branches

//       new Sum.Nested[Identity, F[O], A]:
//         override def branches: Branches[F, O, A] = _branches
//         override def default: Option[A] = None
//         override def metadata: Metadata = Metadata.Empty
//         override def decode(data: Data): Codec.Result[A] = ???
//         override def encode(a: A): Data.Object[Data.String | F[O]] = branches.encodeNested(a, discriminator)

//   // sealed abstract class Merged[+F[+_], +O, A] extends Sum[F, [a] =>> Data.Object[Data.String] | a, O, A]:
//   //   override def orElse[H[+a] >: F[a], P, B](sum: Sum[H, ?, P, B]): Sum.Merged[H, O | P, Either[A, B]] = ???

//   // object Merged:
//   //   def apply[F[+_], O <: Data.Object[P], P, A](
//   //       branches: Branches[F, O, A],
//   //       discriminator: Discriminator.Merged
//   //   ): Sum.Merged[Identity, O, A] =
//   //     val _branches = branches

//   //     new Sum.Merged[Identity, O, A]:
//   //       override def branches: Branches[F, O, A] = _branches
//   //       override def default: Option[A] = None
//   //       override def metadata: Metadata = Metadata.Empty
//   //       override def decode(data: Data): Codec.Result[A] = ???
//   //       override def nested(discriminator: Discriminator.Nested): Sum.Nested[Identity, O, A] = ??? // Nested(branches, discriminator)
//   //       override def encode(a: A): Data.Object[Data.String | P] = branches.encodeMerged[P](a, discriminator)

//   sealed abstract class Untagged[+F[+_], +O, A] extends Sum[F, Identity, O, A]:
//     override def orElse[H[+a] >: F[a], P, B](sum: Sum[H, ?, P, B]): Sum.Untagged[H, O | P, Either[A, B]] = ???

//   object Untagged:
//     def apply[F[+_], O, A](branches: Branches[F, O, A]): Sum.Untagged[Identity, F[O], A] =
//       val _branches = branches

//       new Untagged[Identity, F[O], A]:
//         override def branches: Branches[F, O, A] = _branches
//         override def default: Option[A] = None
//         override def metadata: Metadata = Metadata.Empty
//         override def decode(data: Data): Codec.Result[A] =
//           branches.decodeUntagged(data).andThen(_.toValid(???))
//         override def nested(discriminator: Discriminator.Nested): Nested[Identity, F[O], A] = ???
//         override def encode(a: A): F[O] = branches.encodeUntagged(a)
