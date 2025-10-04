package io.taig.otter

import cats.Eval
import io.taig.otter.operation.NullableOperation

sealed abstract class Nullable[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Nullable[S, T] = Nullable.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A]

object Nullable:
  final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends Nullable[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Modify[S[_], A, B](self: Nullable[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
    export self.schema

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullable[S, Option[A]]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, Option[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Nullable[S, *]] with
    extension [A](self: Nullable[S, A]) override def imap[B](f: A => B)(g: B => A): Nullable[S, B] = self.imap(f)(g)

  given operation[S[_]]: NullableOperation[Nullable[S, *], S] with
    override def nullable[A](value: => S[A]): Nullable[S, Option[A]] = Optional(schema = Reference.later(value))
    override def nullable[A](value: => S[A], default: => A): Nullable[S, A] =
      Default(schema = Reference.later(value), default = Eval.later(default))
