package io.taig.otter

import cats.Invariant
import io.taig.otter.operation.CoerceOperation
import cats.Functor
import cats.Contravariant

sealed abstract class Coerce[+S[_], A] extends Coerce.Read[S, A], Coerce.Write[S, A]:
  def asRead: Coerce.Read[S, A] = this

  def asWrite: Coerce.Write[S, A] = this

  final def imap[T](f: A => T)(g: T => A): Coerce[S, T] = Coerce.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, A]

object Coerce:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Coerce.Read[S, T] = Read.Modify(self = this, f)

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce.Read[T, A]

  object Read:
    final case class Modify[S[_], A, B](self: Coerce.Read[S, A], f: A => B) extends Coerce.Read[S, B]:
      export self.schema

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce.Read[T, B] =
        copy(self = self.mapK[S1, T](fK))

    final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce.Read[S, A]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce.Read[T, A] =
        copy(schema = schema.mapK[S1, T](fK))

    given [S[_]]: Functor[Coerce.Read[S, *]] with
      override def map[A, B](fa: Coerce.Read[S, A])(f: A => B): Read[S, B] = fa.map(f)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](g: T => A): Coerce.Write[S, T] = Write.Modify(self = this, g)

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce.Write[T, A]

  object Write:
    final case class Modify[S[_], A, B](self: Coerce.Write[S, A], g: B => A) extends Coerce.Write[S, B]:
      export self.schema

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce.Write[T, B] =
        copy(self = self.mapK[S1, T](fK))

    final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce.Write[S, A]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce.Write[T, A] =
        copy(schema = schema.mapK[S1, T](fK))

    given [S[_]]: Contravariant[Coerce.Write[S, *]] with
      override def contramap[A, B](fa: Coerce.Write[S, A])(g: B => A): Write[S, B] = fa.contramap(g)

  final case class Modify[S[_], A, B](self: Coerce[S, A], f: A => B, g: B => A) extends Coerce[S, B]:
    export self.schema

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Coerce[S, *]] with
    override def imap[A, B](fa: Coerce[S, A])(f: A => B)(g: B => A): Coerce[S, B] = fa.imap(f)(g)

  // given operation[S[_]]: CoerceOperation[Coerce[S, *], S] with
  //   override def coerce[A](schema: => S[A]): Coerce[S, A] = Root(Reference.later(schema))

  //   override def schema[A](self: Coerce[S, A]): Reference[S, ?] = self.schema
