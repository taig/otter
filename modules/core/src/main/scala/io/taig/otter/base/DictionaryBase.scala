package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Constraint
import io.taig.otter.Dictionary
import io.taig.otter.Reference
import io.taig.validation.Validation

sealed abstract class DictionaryBase[+S[_], A] extends DictionaryBase.Read[S, A], DictionaryBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): DictionaryBase[S, T] = DictionaryBase.Modify(self = this, f, g)

object DictionaryBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): DictionaryBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Object, List[A]])
        extends DictionaryBase.Read[S, List[A]]

    final case class Modify[S[_], A, B](self: DictionaryBase.Read[S, A], f: A => B) extends DictionaryBase.Read[S, B]:
      export self.schema

    given [S[_]]: Functor[DictionaryBase.Read[S, *]] with
      def map[A, B](fa: DictionaryBase.Read[S, A])(f: A => B): DictionaryBase.Read[S, B] = fa.map(f)

    given [S[_]]: Dictionary.Read[DictionaryBase.Read, S] with
      override def linked[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Object, List[A]]
      ): DictionaryBase.Read[T, List[A]] = Linked(schema, validation)

      override def schema[T[a] <: S[a], A](self: DictionaryBase.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): DictionaryBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Linked[S[_], A](schema: Reference[S, A]) extends DictionaryBase.Write[S, List[A]]

    final case class Modify[S[_], A, B](self: DictionaryBase.Write[S, A], f: B => A) extends DictionaryBase.Write[S, B]:
      export self.schema

    given [S[_]]: Contravariant[DictionaryBase.Write[S, *]] with
      def contramap[A, B](fa: DictionaryBase.Write[S, A])(f: B => A): DictionaryBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Dictionary.Write[DictionaryBase.Write, S] with
      override def linked[T[a] <: S[a], A](schema: Reference[T, A]): DictionaryBase.Write[T, List[A]] = Linked(schema)

      override def schema[T[a] <: S[a], A](self: Write[T, A]): Reference[T, ?] = self.schema

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Object, List[A]])
      extends DictionaryBase[S, List[A]]

  final case class Modify[S[_], A, B](self: DictionaryBase[S, A], f: A => B, g: B => A) extends DictionaryBase[S, B]:
    export self.schema

  given [S[_]]: Invariant[DictionaryBase[S, *]] with
    def imap[A, B](fa: DictionaryBase[S, A])(f: A => B)(g: B => A): DictionaryBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Dictionary[DictionaryBase, S] with
    override def linked[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Object, List[A]]
    ): DictionaryBase[T, List[A]] = Linked(schema, validation)

    override def schema[T[a] <: S[a], A](self: DictionaryBase[T, A]): Reference[T, ?] = self.schema
