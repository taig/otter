package io.taig.otter.base

import cats.data.Chain
import io.taig.otter.Record
import io.taig.otter.Reference
import cats.Invariant
import cats.Functor
import cats.Contravariant

sealed abstract class RecordBase[+S[+_[a] <: T[a], _], +T[_], A]
    extends RecordBase.Read[S, T, A],
      RecordBase.Write[S, T, A]:
  final def imap[B](f: A => B)(g: B => A): RecordBase[S, T, B] = RecordBase.Modify(self = this, f, g)

  final def zip[U[+s[a] <: V[a], a] >: S[s, a], V[a] >: T[a], B](schema: RecordBase[U, V, B]): RecordBase[U, V, (A, B)] = 
    RecordBase.Zip[U, V, A, B](left = this, right = schema)

object RecordBase:
  sealed trait Read[+S[+_[a] <: T[a], _], +T[_], +A] extends Product, Serializable:
    def fields: Chain[Reference[S[T, *], ?]]

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

  sealed trait Write[+S[+_[a] <: T[a], _], +T[_], -A] extends Product, Serializable:
    def fields: Chain[Reference[S[T, *], ?]]

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

  case object Empty extends RecordBase[Nothing, Nothing, Unit]:
    override def fields: Chain[Reference[Nothing, ?]] = Chain.empty

  final case class Modify[S[+_[a] <: T[a], _], T[_], A, B](self: RecordBase[S, T, A], f: A => B, g: B => A)
      extends RecordBase[S, T, B]:
    export self.fields

  final case class Root[S[+_[_], _], T[_], A](field: Reference[S[T, *], A]) extends RecordBase[S, T, A]:
    override def fields: Chain[Reference[S[T, *], ?]] = Chain.one(field)

  final case class Zip[S[+_[_], _], +T[_], A, B](left: RecordBase[S, T, A], right: RecordBase[S, T, B])
      extends RecordBase[S, T, (A, B)]:
    override def fields: Chain[Reference[S[T, *], ?]] = left.fields ++ right.fields

  given [S[+_[_], _], T[_]]: Invariant[RecordBase[S, T, *]] with
    def imap[A, B](fa: RecordBase[S, T, A])(f: A => B)(g: B => A): RecordBase[S, T, B] = fa.imap(f)(g)

  given [S[+_[_], _], T[_]]: Record[[s[a] <: T[a], a] =>> RecordBase[S, s, a], S, T] with
    override def apply[I[a] <: T[a], A](field: Reference[S[I, *], A]): RecordBase[S, I, A] = Root(field)

    override def empty: RecordBase[S, Nothing, Unit] = Empty

    extension [I[a] <: T[a], A](self: RecordBase[S, I, A])
      override def fields: Chain[Reference[S[I, *], ?]] = self.fields

      override def zip[J[a] >: I[a] <: T[a], B](schema: RecordBase[S, J, B]): RecordBase[S, J, (A, B)] =
        self.zip(schema)
