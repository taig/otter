package io.taig.otter

import cats.Invariant
import cats.data.Chain
import io.taig.otter.operation.RecordOperation
import cats.Eval

sealed abstract class Record[+S[_], A] extends Product, Serializable:
  def fields: Chain[Field[S, ?]]

  final def imap[T](f: A => T)(g: T => A): Record[S, T] = Record.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A]

  final def zip[T[_], B](schema: Record[T, B]): Record[[a] =>> S[a] | T[a], (A, B)] =
    Record.Zip(left = this, right = schema)

object Record:
  final case class Default[S[_], A](self: Record[S, A], value: Eval[A]) extends Record[S, A]:
    export self.fields

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A] =
      copy(self = self.mapK[S1, T](fK))

  case object Empty extends Record[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty

    override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Record[T, Unit] = this

  final case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.fields

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](self: Record[S, A]) extends Record[S, Option[A]]:
    export self.fields

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, Option[A]] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](field: Field[S, A]) extends Record[S, A]:
    override def fields: Chain[Field[S, A]] = Chain.one(field)

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A] =
      copy(field = field.mapK[S1, T](fK))

  final case class Zip[S[_], T[_], A, B](left: Record[S, A], right: Record[T, B])
      extends Record[[a] =>> S[a] | T[a], (A, B)]:
    override def fields: Chain[Field[[a] =>> S[a] | T[a], ?]] = left.fields ++ right.fields

    override def mapK[S1[a] >: (S[a] | T[a]), U[_]](fK: [A] => S1[A] => U[A]): Record[U, (A, B)] =
      copy(left = left.mapK[S1, U](fK), right = right.mapK[S1, U](fK))

  given invariant[S[_]]: Invariant[Record[S, *]] with
    override def imap[A, B](fa: Record[S, A])(f: A => B)(g: B => A): Record[S, B] = fa.imap(f)(g)

  given operation[S[_]]: RecordOperation[Record[S, *], S] with
    override def empty: Record[S, Unit] = Empty

    override def fields[A](self: Record[S, A]): Chain[Field[S, ?]] = self.fields

    override def lift[A](field: Field[S, A]): Record[S, A] = Root(field)

    override def zip[A, B](left: Record[S, A], right: Record[S, B]): Record[S, (A, B)] = Zip(left, right)
