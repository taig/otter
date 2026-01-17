package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

sealed abstract class Dictionary[+S[_], A] extends Dictionary.Read[S, A], Dictionary.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Dictionary[S, B] = Dictionary.Modify(self = this, f, g)

  def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary[G, A]

object Dictionary:
  sealed trait Read[+S[_], +A]:
    def schema: Reference[S, ?]

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary.Read[G, A]

    final def map[B](f: A => B): Dictionary.Read[S, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: Dictionary.Read[S, A], f: A => B) extends Dictionary.Read[S, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary.Read[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Functor[Dictionary.Read[F, *]]:
      override def map[A, B](fa: Dictionary.Read[F, A])(f: A => B): Dictionary.Read[F, B] = fa.map(f)

    given [F[_]] => DictionaryOperation.Read[Dictionary.Read[F, *], F]:
      override def hashed[A](
          schema: Reference[F, A],
          validation: Validation[Constraint.Object, SortedMap[String, A]]
      ): Dictionary.Read[F, SortedMap[String, A]] = Hashed(schema, validation)

      override def linked[A](
          schema: Reference[F, A],
          validation: Validation[Constraint.Object, List[(String, A)]]
      ): Dictionary.Read[F, List[(String, A)]] = Linked(schema, validation)

      extension [A](fa: Dictionary.Read[F, A]) override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+S[_], -A]:
    def schema: Reference[S, ?]

    def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary.Write[G, A]

    final def contramap[B](f: B => A): Dictionary.Write[S, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Dictionary.Write[S, A], f: B => A) extends Dictionary.Write[S, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary.Write[G, B] = copy(self = self.mapK(fK))

    given [F[_]] => Contravariant[Dictionary.Write[F, *]]:
      override def contramap[A, B](fa: Dictionary.Write[F, A])(f: B => A): Dictionary.Write[F, B] = fa.contramap(f)

    given [F[_]] => DictionaryOperation.Write[Dictionary.Write[F, *], F]:
      override def hashed[A](schema: Reference[F, A]): Dictionary.Write[F, SortedMap[String, A]] =
        Hashed(schema, validation = Validation.valid)

      override def linked[A](schema: Reference[F, A]): Dictionary.Write[F, List[(String, A)]] =
        Linked(schema, validation = Validation.valid)

      extension [A](fa: Dictionary.Write[F, A]) override def schema: Reference[F, ?] = fa.schema

  final case class Hashed[S[_], A](
      schema: Reference[S, A],
      validation: Validation[Constraint.Object, SortedMap[String, A]]
  ) extends Dictionary[S, SortedMap[String, A]]:
    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary[G, SortedMap[String, A]] =
      copy(schema = schema.mapK[S, G](fK))

  final case class Linked[S[_], A](
      schema: Reference[S, A],
      validation: Validation[Constraint.Object, List[(String, A)]]
  ) extends Dictionary[S, List[(String, A)]]:
    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary[G, List[(String, A)]] =
      copy(schema = schema.mapK[S, G](fK))

  final case class Modify[S[_], A, B](self: Dictionary[S, A], f: A => B, g: B => A) extends Dictionary[S, B]:
    export self.schema

    override def mapK[G[_]](fK: [A] => S[A] => G[A]): Dictionary[G, B] = copy(self = self.mapK(fK))

  given [F[_]] => Invariant[Dictionary[F, *]]:
    override def imap[A, B](self: Dictionary[F, A])(f: A => B)(g: B => A): Dictionary[F, B] = self.imap(f)(g)

  given [F[_]] => DictionaryOperation[Dictionary[F, *], F]:
    override def hashed[A](
        schema: Reference[F, A],
        validation: Validation[Constraint.Object, SortedMap[String, A]]
    ): Dictionary[F, SortedMap[String, A]] = Hashed(schema, validation)

    override def linked[A](
        schema: Reference[F, A],
        validation: Validation[Constraint.Object, List[(String, A)]]
    ): Dictionary[F, List[(String, A)]] = Linked(schema, validation)

    extension [A](fa: Dictionary[F, A]) override def schema: Reference[F, ?] = fa.schema
