package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.schema.NullableSchema

sealed abstract class Nullable[+S[_], A] extends Product with Serializable:
  def schema: Option[Reference[S, ?]]

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A]
  final def imap[B](f: A => B)(g: B => A): Nullable[S, B] = Nullable.Modify(self = this, f, g)

object Nullable:
  final private[otter] case class Modify[S[_], A, B](self: Nullable[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
    export self.schema
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Default[S[_], A](reference: Reference[S, A], default: A) extends Nullable[S, A]:
    override def schema: Option[Reference[S, ?]] = reference.some
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, A] =
      copy(reference = reference.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](reference: Reference[S, A]) extends Nullable[S, Option[A]]:
    override def schema: Option[Reference[S, ?]] = reference.some
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, Option[A]] =
      copy(reference = reference.mapK[S1, T](fK))

  private[otter] case object Void extends Nullable[Nothing, Unit]:
    override def schema: Option[Reference[Nothing, ?]] = none
    override def mapK[S1[a] >: Nothing, T[_]](fK: [A] => S1[A] => T[A]): Nullable[T, Unit] = this

  given [Value[_]]: NullableSchema[Nullable[Value, *], Value] with
    override def apply[A](schema: => Value[A]): Nullable[Value, Option[A]] =
      Root(reference = Reference.later(schema))
    override def apply[A](schema: => Value[A], default: A): Nullable[Value, A] =
      Default(reference = Reference.later(schema), default)
    override def void: Nullable[Nothing, Unit] = Nullable.Void

    override def imap[A, B](fa: Nullable[Value, A])(f: A => B)(g: B => A): Nullable[Value, B] = fa.imap(f)(g)
