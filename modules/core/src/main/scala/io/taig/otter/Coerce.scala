package io.taig.otter

import io.taig.otter.operation.CoerceOperation

sealed abstract class Coerce[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Coerce[S, T] = Coerce.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, A]

object Coerce:
  final case class Modify[S[_], A, B](self: Coerce[S, A], f: A => B, g: B => A) extends Coerce[S, B]:
    export self.schema

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Coerce[S, *]] with
    extension [A](self: Coerce[S, A]) override def imap[B](f: A => B)(g: B => A): Coerce[S, B] = self.imap(f)(g)

  given operation[S[_]]: CoerceOperation[Coerce[S, *], S] with
    override def coerce[A](schema: => S[A]): Coerce[S, A] = Root(schema = Reference.later(schema))
