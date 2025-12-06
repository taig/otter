package io.taig.otter.base

import cats.data.Chain
import io.taig.otter.Record
import io.taig.otter.Reference
import cats.Invariant
import cats.Functor
import cats.Contravariant

sealed abstract class RecordBase[+S[_], A] extends RecordBase.Read[S, A], RecordBase.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): RecordBase[S, B] = ??? // RecordBase.Modify(self = this, f, g)

  // final def zip[U[+s[a] <: T[a], a] >: S[s, a], V[a] >: T[a], B](schema: RecordBase[U, V, B]): RecordBase[U, V, (A, B)] =
  //   // val x: RecordBase[S, T, A] = this
  //   RecordBase.Zip[U, V, A, B](left = ???, right = schema)

object RecordBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

//     final def map[B](f: A => B): RecordBase.Read[S, T, B] = Read.Modify(self = this, f)

//     final def zip[U[+s[_], a] >: S[s, a], V[a] >: T[a], B](
//         schema: RecordBase.Read[U, V, B]
//     ): RecordBase.Read[U, V, (A, B)] = Read.Zip(left = this, right = schema)

//   object Read:
//     final case class Modify[S[+_[_], _], T[_], A, B](self: RecordBase.Read[S, T, A], f: A => B) extends Read[S, T, B]:
//       export self.fields

//     final case class Zip[S[+_[_], _], T[_], A, B](left: RecordBase.Read[S, T, A], right: RecordBase.Read[S, T, B])
//         extends Read[S, T, (A, B)]:
//       override def fields: Chain[Reference[S[T, *], ?]] = left.fields ++ right.fields

//     given [S[+_[_], _], T[_]]: Functor[RecordBase.Read[S, T, *]] with
//       def map[A, B](fa: RecordBase.Read[S, T, A])(f: A => B): RecordBase.Read[S, T, B] = fa.map(f)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

//     final def contramap[B](f: B => A): RecordBase.Write[S, T, B] = Write.Modify(self = this, f)

//     final def zip[U[+s[_], a] >: S[s, a], V[a] >: T[a], B](
//         schema: RecordBase.Write[U, V, B]
//     ): RecordBase.Write[U, V, (A, B)] =
//       Write.Zip(left = this, right = schema)

//   object Write:
//     final case class Modify[S[+_[_], _], T[_], A, B](self: RecordBase.Write[S, T, A], f: B => A) extends Write[S, T, B]:
//       export self.fields

//     final case class Zip[S[+_[_], _], T[_], A, B](left: RecordBase.Write[S, T, A], right: RecordBase.Write[S, T, B])
//         extends Write[S, T, (A, B)]:
//       override def fields: Chain[Reference[S[T, *], ?]] = left.fields ++ right.fields

//     given [S[+_[_], _], T[_]]: Contravariant[RecordBase.Write[S, T, *]] with
//       def contramap[A, B](fa: RecordBase.Write[S, T, A])(f: B => A): RecordBase.Write[S, T, B] = fa.contramap(f)

  case object Empty extends RecordBase[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty

  final case class Root[S[_], A](field: Reference[S, A]) extends RecordBase[S, A]:
    override def fields: Chain[Reference[S, ?]] = Chain.one(field)

  // given [S[_]]: Invariant[RecordBase[S, *]] with
  //   def imap[A, B](fa: RecordBase[S, A])(f: A => B)(g: B => A): RecordBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Record[RecordBase, S] with
    override def apply[I[a] <: S[a], A](field: Reference[I, A]): RecordBase[I, A] = Root(field)

    override def empty: RecordBase[Nothing, Unit] = Empty

  //   override def apply[I[a] <: T[a], A](field: Reference[S[I, *], A]): RecordBase[S, I, A] = Root(field)

  //   override def empty: RecordBase[S, Nothing, Unit] = Empty

  //   extension [I[a] <: T[a], A](self: RecordBase[S, I, A])
  //     override def fields: Chain[Reference[S[I, *], ?]] = self.fields

  //     override def zip[J[a] >: I[a] <: T[a], B](schema: RecordBase[S, J, B]): RecordBase[S, J, (A, B)] =
  //       self.zip(schema)
