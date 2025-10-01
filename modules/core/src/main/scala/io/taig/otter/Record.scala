package io.taig.otter

import cats.data.Chain

sealed abstract class Record[+S[_], A] extends Product with Serializable:
  def fields: Chain[Reference[S, ?]]

  final def imap[T](f: A => T)(g: T => A): Record[S, T] = Record.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A]

  final def zip[S1[a] >: S[a], B](schema: Record[S1, B]): Record[S1, (A, B)] =
    Record.Zip(left = this, right = schema)

object Record:
  case object Empty extends Record[Nothing, Unit]:
    override def fields: Chain[Reference[Nothing, ?]] = Chain.empty
    override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Record[T, Unit] = this

  final case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.fields
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](field: Reference[S, A]) extends Record[S, A]:
    override def fields: Chain[Reference[S, ?]] = Chain.one(field)
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, A] =
      copy(field = field.mapK[S1, T](fK))

  final case class Zip[S[_], A, B](left: Record[S, A], right: Record[S, B]) extends Record[S, (A, B)]:
    override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Record[T, (A, B)] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))
