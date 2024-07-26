package io.taig.otter

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Data.Optional
import cats.Id as Identity

sealed abstract class Sum[+F[+_], +O <: Data, A] extends Codec[F, O, A], Sum.Reader[F, O, A], Sum.Writer[F, O, A]:
  override def branches: NonEmptyChain[Branch[?, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Sum[F, O, A] = ???
  override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum[F, O, A1] = ???
  override def imap[B](f: A => B)(g: B => A): Sum[F, O, B]
  override def optional: Sum[Data.Optional, O, Option[A]] = ???
  override def nested(discriminator: Discriminator.Nested): Sum.Nested[F, Data.Object[? >: Data.String], A]
//   def untagged: Sum.Untagged[F, Data, A]

object Sum:
  abstract class Nested[+F[+_], +O <: Data, A]
      extends Sum[F, Data.Object[Data.String | O], A],
        Sum.Nested.Reader[F, O, A],
        Sum.Nested.Writer[F, O, A]:
    override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Nested[F, O, A1] = ???
    override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = ???
    override def map[B](f: A => B): Sum.Nested[F, O, B] = ???
    override def optional: Sum.Nested[Optional, O, Option[A]] = ???

  object Nested:
    sealed trait Reader[+F[+_], +O <: Data, +A] extends Sum.Reader[F, Data.Object[Data.String | O], A]:
      override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Nested.Reader[F, O, A1]
      override def modifyMetadata(f: Metadata => Metadata): Sum.Nested.Reader[F, O, A] = ???
      override def map[B](f: A => B): Sum.Nested.Reader[F, O, B] = ???
      override def optional: Sum.Nested.Reader[Data.Optional, O, Option[A]] = ???

    sealed trait Writer[+F[+_], +O <: Data, -A] extends Sum.Writer[F, Data.Object[Data.String | O], A]:
      override def modifyMetadata(f: Metadata => Metadata): Sum.Nested.Writer[F, O, A] = ???
      override def contramap[B](f: B => A): Sum.Nested.Writer[F, O, B] = ???
      override def optional: Sum.Nested.Writer[Data.Optional, O, Option[A]] = ???

  sealed abstract class Merged[+F[+_], +O <: Data, A] extends Sum[F, Data.Object[Data.String | O], A], Sum.Merged.Reader[F, O, A], Sum.Merged.Writer[F, O, A]:
    override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Merged[F, O, A1] = ???
    override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???
    override def optional: Sum.Merged[Optional, O, Option[A]] = ???

  object Merged:
    sealed trait Reader[+F[+_], +O <: Data, +A] extends Sum.Reader[F, Data.Object[Data.String | O], A]:
      override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Merged.Reader[F, O, A1] = ???
      override def modifyMetadata(f: Metadata => Metadata): Sum.Merged.Reader[F, O, A] = ???
      override def map[B](f: A => B): Sum.Merged.Reader[F, O, B] = ???
      override def optional: Sum.Merged.Reader[Optional, O, Option[A]] = ???

    sealed trait Writer[+F[+_], +O <: Data, -A] extends Sum.Writer[F, Data.Object[Data.String | O], A]:
      override def modifyMetadata(f: Metadata => Metadata): Sum.Merged.Writer[F, O, A] = ???
      override def contramap[B](f: B => A): Sum.Merged.Writer[F, O, B] = ???
      override def optional: Sum.Merged.Writer[Optional, O, Option[A]] = ???

  sealed trait Keyed[+F[+_], +O <: Data, A] extends Sum[F, Data.Object[O], A], Sum.Keyed.Reader[F, O, A], Sum.Keyed.Writer[F, O, A]:
    override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Keyed[F, O, A1] = ???
    override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = ???
    override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = ???
    override def optional: Sum.Keyed[Optional, O, Option[A]] = ???

  object Keyed:
    sealed trait Reader[+F[+_], +O <: Data, +A] extends Sum.Reader[F, Data.Object[O], A]:
      override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Keyed.Reader[F, O, A1] = ???
      override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed.Reader[F, O, A] = ???
      override def map[B](f: A => B): Sum.Keyed.Reader[F, O, B] = ???
      override def optional: Sum.Keyed.Reader[Optional, O, Option[A]] = ???
    
    sealed trait Writer[+F[+_], +O <: Data, -A] extends Sum.Writer[F, Data.Object[O], A]:
      override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed.Writer[F, O, A] = ???
      override def contramap[B](f: B => A): Sum.Keyed.Writer[F, O, B] = ???
      override def optional: Sum.Keyed.Writer[Optional, O, Option[A]] = ???

  sealed trait Untagged[+F[+_], +O <: Data, A] extends Sum[F, O, A], Sum.Untagged.Reader[F, O, A], Sum.Untagged.Writer[F, O, A]

  object Untagged:
    sealed trait Reader[+F[+_], +O <: Data, +A] extends Sum.Reader[F, O, A]
    
    sealed trait Writer[+F[+_], +O <: Data, -A] extends Sum.Writer[F, O, A]

  sealed trait Reader[+F[+_], +O <: Data, +A] extends Codec.Reader[F, O, A]:
    def branches: NonEmptyChain[Branch.Reader[?, ?]]
    override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Sum.Reader[F, O, A1]
    override def modifyMetadata(f: Metadata => Metadata): Sum.Reader[F, O, A]
    override def map[B](f: A => B): Sum.Reader[F, O, B]
    override def optional: Sum.Reader[Optional, O, Option[A]] = ???
    def nested(discriminator: Discriminator.Nested): Sum.Nested.Reader[F, Data.Object[? >: Data.String], A]

  sealed trait Writer[+F[+_], +O <: Data, -A] extends Codec.Writer[F, O, A]:
    def branches: NonEmptyChain[Branch.Writer[?, ?]]
    override def modifyMetadata(f: Metadata => Metadata): Sum.Writer[F, O, A]
    override def contramap[B](f: B => A): Sum.Writer[F, O, B]
    override def optional: Sum.Writer[Optional, O, Option[A]]
    def nested(discriminator: Discriminator.Nested): Sum.Nested.Writer[F, Data.Object[? >: Data.String], A]

//   sealed abstract class Nested[+F[+a] <: Data.Optional[a], +O <: Data, A]
//       extends Sum[F, Data.Object[Data.String | O], A]:
//     override def modifyMetadata(f: Metadata => Metadata): Sum.Nested[F, O, A] = ???
//     override def modifyDefault(f: Option[A] => Option[A]): Sum.Nested[F, O, A] = ???
//     override def imap[B](f: A => B)(g: B => A): Sum.Nested[F, O, B] = ???
//     override def optional: Sum.Nested[Data.Optional, O, Option[A]] = ???
//     override def orElse[B](sum: Sum[?, ?, B]): Sum.Nested[Identity, O, Either[A, B]] = ???
//     override def untagged: Sum.Untagged[F, O, A]

//   object Nested:
//     def apply[O <: Data, A](branch: Branch[O, A], discriminator: Discriminator.Nested): Sum.Nested[Identity, O, A] =
//       new Nested[Identity, O, A]:
//         override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//         override def metadata: Metadata = Metadata.Empty
//         override def default: Option[A] = None
//         override def untagged: Untagged[Identity, O, A] = Untagged(branch)
//         override def decode(data: Data): Codec.Result[A] = ???
//         override def encode(a: A): Data.Object[Data.String | O] =
//           Data.Object.of(discriminator.value -> branch.encode(a), discriminator.identifier -> Data.String(branch.name))

//   sealed abstract class Merged[+F[+a] <: Data.Optional[a], +O <: Data, A]
//       extends Sum[F, Data.Object[Data.String | O], A]:
//     override def modifyMetadata(f: Metadata => Metadata): Sum.Merged[F, O, A] = ???
//     override def modifyDefault(f: Option[A] => Option[A]): Sum.Merged[F, O, A] = ???
//     override def imap[B](f: A => B)(g: B => A): Sum.Merged[F, O, B] = ???
//     override def optional: Sum.Merged[Data.Optional, O, Option[A]] = ???
//     override def orElse[B](sum: Sum[?, ?, B]): Sum.Merged[Identity, O, Either[A, B]] = ???
//     override def untagged: Sum.Untagged[F, Data.Object[O], A]

//   object Merged:
//     def apply[O <: Data.Object[P], P <: Data, A](
//         branch: Branch[O, A],
//         discriminator: Discriminator.Merged
//     ): Sum.Merged[Identity, P, A] = new Merged[Identity, P, A]:
//       override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//       override def metadata: Metadata = Metadata.Empty
//       override def default: Option[A] = None
//       override def untagged: Untagged[Identity, Data.Object[P], A] = Untagged(branch)
//       override def decode(data: Data): Codec.Result[A] = ???
//       override def encode(a: A): O & Data.Object[Data.String] =
//         ??? // Data.Object.one(discriminator.identifier, Data.String(branch.name)) ++ branch.encode(a)

//   sealed abstract class Keyed[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, Data.Object[O], A]:
//     override def modifyMetadata(f: Metadata => Metadata): Sum.Keyed[F, O, A] = ???
//     override def modifyDefault(f: Option[A] => Option[A]): Sum.Keyed[F, O, A] = ???
//     override def imap[B](f: A => B)(g: B => A): Sum.Keyed[F, O, B] = ???
//     override def optional: Sum.Keyed[Data.Optional, O, Option[A]] = ???
//     override def untagged: Sum.Untagged[F, O, A]

//   object Keyed:
//     def apply[O <: Data, A](branch: Branch[O, A]): Sum.Keyed[Identity, O, A] = new Keyed[Identity, O, A]:
//       override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//       override def metadata: Metadata = Metadata.Empty
//       override def default: Option[A] = None
//       override def untagged: Sum.Untagged[Identity, O, A] = Untagged(branch)
//       override def decode(data: Data): Codec.Result[A] = ???
//       override def encode(a: A): Data.Object[O] = Data.Object.one(branch.name, branch.encode(a))

//   sealed abstract class Untagged[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Sum[F, O, A]:
//     override def modifyMetadata(f: Metadata => Metadata): Sum.Untagged[F, O, A] = ???
//     override def modifyDefault(f: Option[A] => Option[A]): Sum.Untagged[F, O, A] = ???
//     override def imap[B](f: A => B)(g: B => A): Sum.Untagged[F, O, B] = ???
//     override def optional: Sum.Untagged[Data.Optional, O, Option[A]] = ???
//     final override def untagged: Sum.Untagged[F, O, A] = this

//   object Untagged:
//     def apply[O <: Data, A](branch: Branch[O, A]): Sum.Untagged[Identity, O, A] = new Untagged[Identity, O, A]:
//       override def branches: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
//       override def metadata: Metadata = Metadata.Empty
//       override def default: Option[A] = None
//       override def decode(data: Data): Codec.Result[A] = branch.decode(data)
//       override def encode(a: A): O = branch.encode(a)
