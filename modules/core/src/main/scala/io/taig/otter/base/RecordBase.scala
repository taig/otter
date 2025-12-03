package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.Record
import io.taig.otter.Reference
import java.sql.Ref
import io.taig.otter.Field

sealed abstract class RecordBase[+S[_], A] extends RecordBase.Read[S, A], RecordBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): RecordBase[S, T] = RecordBase.Modify(self = this, f, g)

  final def zip[T[_], B](schema: RecordBase[T, B]): RecordBase[[a] =>> S[a] | T[a], (A, B)] =
    RecordBase.Zip(left = this, right = schema)

object RecordBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def fields: Chain[Field[S, ?]]

    final def map[T](f: A => T): RecordBase.Read[S, T] = Read.Modify(self = this, f)

    final def zip[T[_], B](schema: RecordBase.Read[T, B]): RecordBase.Read[[a] =>> S[a] | T[a], (A, B)] =
      Read.Zip(left = this, right = schema)

  object Read:
    case object Empty extends RecordBase.Read[Nothing, Unit]:
      override def fields: Chain[Nothing] = Chain.empty

    final case class Modify[S[_], A, B](self: RecordBase.Read[S, A], f: A => B) extends RecordBase.Read[S, B]:
      export self.fields

    final case class Zip[S[_], T[_], A, B](left: RecordBase.Read[S, A], right: RecordBase.Read[T, B])
        extends RecordBase.Read[[a] =>> S[a] | T[a], (A, B)]:
      override def fields: Chain[Field[[a] =>> S[a] | T[a], ?]] = left.fields ++ right.fields

    given [S[_]]: Functor[RecordBase.Read[S, *]] with
      def map[A, B](fa: RecordBase.Read[S, A])(f: A => B): RecordBase.Read[S, B] = fa.map(f)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def fields: Chain[Field[S, ?]]

    final def contramap[T](f: T => A): RecordBase.Write[S, T] = Write.Modify(self = this, f)

    final def zip[T[_], B](schema: RecordBase.Write[T, B]): RecordBase.Write[[a] =>> S[a] | T[a], (A, B)] =
      Write.Zip(left = this, right = schema)

  object Write:
    case object Empty extends RecordBase.Write[Nothing, Unit]:
      override def fields: Chain[Nothing] = Chain.empty

    final case class Modify[S[_], A, B](self: RecordBase.Write[S, A], f: B => A) extends RecordBase.Write[S, B]:
      export self.fields

    final case class Zip[S[_], T[_], A, B](left: RecordBase.Write[S, A], right: RecordBase.Write[T, B])
        extends RecordBase.Write[[a] =>> S[a] | T[a], (A, B)]:
      override def fields: Chain[Field[[a] =>> S[a] | T[a], ?]] = left.fields ++ right.fields

    given [S[_]]: Contravariant[RecordBase.Write[S, *]] with
      def contramap[A, B](fa: RecordBase.Write[S, A])(f: B => A): RecordBase.Write[S, B] = fa.contramap(f)

  case object Empty extends RecordBase[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty

  final case class Root[S[_], A](field: Field[S, A]) extends RecordBase[S, A]:
    override def fields: Chain[Field[S, A]] = Chain.one(field)

  final case class Modify[S[_], A, B](self: RecordBase[S, A], f: A => B, g: B => A) extends RecordBase[S, B]:
    export self.fields

  final case class Zip[S[_], T[_], A, B](left: RecordBase[S, A], right: RecordBase[T, B])
      extends RecordBase[[a] =>> S[a] | T[a], (A, B)]:
    override def fields: Chain[Field[[a] =>> S[a] | T[a], ?]] = left.fields ++ right.fields

  given [S[_]]: Invariant[RecordBase[S, *]] with
    def imap[A, B](fa: RecordBase[S, A])(f: A => B)(g: B => A): RecordBase[S, B] = fa.imap(f)(g)
