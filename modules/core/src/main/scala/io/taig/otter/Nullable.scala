package io.taig.otter

import cats.Eval

sealed abstract class Nullable[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Nullable[S, T] = Nullable.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A]

object Nullable:
  final case class Default[S[_], A](reference: Reference[S, A], default: Eval[A]) extends Nullable[S, A]:
    def schema: Reference[S, ?] = reference

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A] =
      copy(reference = reference.mapK[S1, T](fK))

  final case class Modify[S[_], A, B](self: Nullable[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
    export self.schema

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](reference: Reference[S, A]) extends Nullable[S, Option[A]]:
    override def schema: Reference[S, ?] = reference
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, Option[A]] =
      copy(reference = reference.mapK[S1, T](fK))
