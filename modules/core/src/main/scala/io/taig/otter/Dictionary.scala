package io.taig.otter

import io.taig.validation.Validation
import io.taig.otter.operation.DictionaryOperation

sealed abstract class Dictionary[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Dictionary[S, T] = Dictionary.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Dictionary[T, A]

object Dictionary:
  final case class Modify[S[_], A, B](self: Dictionary[S, A], f: A => B, g: B => A) extends Dictionary[S, B]:
    export self.schema

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Dictionary[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Object, A])
      extends Dictionary[S, List[(String, A)]]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Dictionary[T, List[(String, A)]] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Dictionary[S, *]] with
    extension [A](self: Dictionary[S, A])
      override def imap[B](f: A => B)(g: B => A): Dictionary[S, B] =
        self.imap(f)(g)

  given operation[S[_]]: DictionaryOperation[Dictionary[S, *], S] with
    override def dictionary[A](
        schema: => S[A],
        validation: Validation[Constraint.Object, A]
    ): Dictionary[S, List[(String, A)]] = Root(schema = Reference.later(schema), validation)
