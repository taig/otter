// package io.taig.otter

// import cats.syntax.all.*
// import cats.~>

// sealed abstract class Nullable[+S[_], A]:
//   def metadata: Metadata
//   def modifyMetadata(f: Metadata => Metadata): Nullable[S, A]
//   def codec: Option[Reference[S, ?]]
//   def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Nullable[T, A]
//   final def imap[B](f: A => B)(g: B => A): Nullable[S, B] = Nullable.Modify(self = this, f, g)

// object Nullable:
//   final private[otter] case class Modify[S[_], A, B](self: Nullable[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
//     export self.{codec, metadata}
//     override def modifyMetadata(f: Metadata => Metadata): Nullable[S, B] = copy(self = self.modifyMetadata(f))
//     override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Nullable[T, B] = copy(self = self.mapK(fK))

//   final private[otter] case class Default[S[_], A](reference: Reference[S, A], default: A, metadata: Metadata)
//       extends Nullable[S, A]:
//     override def codec: Option[Reference[S, ?]] = reference.some
//     override def modifyMetadata(f: Metadata => Metadata): Nullable[S, A] = copy(metadata = f(metadata))
//     override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Nullable[T, A] = copy(reference = reference.mapK(fK))

//   final private[otter] case class Root[S[_], A](reference: Reference[S, A], metadata: Metadata)
//       extends Nullable[S, Option[A]]:
//     override def codec: Option[Reference[S, ?]] = reference.some
//     override def modifyMetadata(f: Metadata => Metadata): Nullable[S, Option[A]] = copy(metadata = f(metadata))
//     override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Nullable[T, Option[A]] = copy(reference = reference.mapK(fK))

//   final private[otter] case class Void(metadata: Metadata) extends Nullable[Nothing, Unit]:
//     override def codec: Option[Reference[Nothing, ?]] = none
//     override def modifyMetadata(f: Metadata => Metadata): Nullable[Nothing, Unit] = copy(metadata = f(metadata))
//     override def mapK[S1[a] >: Nothing, T[_]](fK: S1 ~> T): Nullable[T, Unit] = this
