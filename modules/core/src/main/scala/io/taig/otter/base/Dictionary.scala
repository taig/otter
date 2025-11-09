package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter as Self
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

sealed abstract class Dictionary[+S[_], A] extends Dictionary.Read[S, A], Dictionary.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Dictionary[S, T] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Dictionary.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Object, List[A]])
        extends Dictionary.Read[S, List[A]]

    final case class Modify[S[_], A, B](self: Dictionary.Read[S, A], f: A => B) extends Dictionary.Read[S, B]:
      export self.schema

    given [S[_]]: Functor[Dictionary.Read[S, *]] with
      def map[A, B](fa: Dictionary.Read[S, A])(f: A => B): Dictionary.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Dictionary.Read[Dictionary.Read, S] = new Self.Dictionary.Read[Dictionary.Read, S]:
      override def linked[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Object, List[A]]
      ): Dictionary.Read[T, List[A]] = Linked(schema, validation)

      override def schema[T[a] <: S[a], A](self: Dictionary.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Dictionary.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Linked[S[_], A](schema: Reference[S, A]) extends Dictionary.Write[S, List[A]]

    final case class Modify[S[_], A, B](self: Dictionary.Write[S, A], f: B => A) extends Dictionary.Write[S, B]:
      export self.schema

    given [S[_]]: Contravariant[Dictionary.Write[S, *]] with
      def contramap[A, B](fa: Dictionary.Write[S, A])(f: B => A): Dictionary.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Dictionary.Write[Dictionary.Write, S] = new Self.Dictionary.Write[Dictionary.Write, S]:
      override def linked[T[a] <: S[a], A](schema: Reference[T, A]): Dictionary.Write[T, List[A]] = Linked(schema)

      override def schema[T[a] <: S[a], A](self: Write[T, A]): Reference[T, ?] = self.schema

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Object, List[A]])
      extends Dictionary[S, List[A]]

  final case class Modify[S[_], A, B](self: Dictionary[S, A], f: A => B, g: B => A) extends Dictionary[S, B]:
    export self.schema

  given [S[_]]: Invariant[Dictionary[S, *]] with
    def imap[A, B](fa: Dictionary[S, A])(f: A => B)(g: B => A): Dictionary[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Dictionary[Dictionary, S] = new Self.Dictionary[Dictionary, S]:
    override def linked[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Object, List[A]]
    ): Dictionary[T, List[A]] = Linked(schema, validation)

    override def schema[T[a] <: S[a], A](self: Dictionary[T, A]): Reference[T, ?] = self.schema
