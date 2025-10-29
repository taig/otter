package io.taig.otter

import cats.Invariant
import cats.data.Chain
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Validation

sealed abstract class Dictionary[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  def constraints: Chain[Constraint.Object]

  final def imap[T](f: A => T)(g: T => A): Dictionary[S, T] = Dictionary.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Dictionary[T, A]

object Dictionary:
  final case class Modify[S[_], A, B](self: Dictionary[S, A], f: A => B, g: B => A) extends Dictionary[S, B]:
    export self.{constraints, schema}

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Dictionary[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](
      schema: Reference[S, A],
      validation: Validation[Constraint.Object, List[(String, A)]]
  ) extends Dictionary[S, List[(String, A)]]:
    override def constraints: Chain[Constraint.Object] = validation.constraints

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Dictionary[T, List[(String, A)]] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Dictionary[S, *]] with
    override def imap[A, B](fa: Dictionary[S, A])(f: A => B)(g: B => A): Dictionary[S, B] = fa.imap(f)(g)

  given operation[S[_]]: DictionaryOperation[Dictionary[S, *], S] with
    override def dictionary[A](
        schema: => S[A],
        validation: Validation[Constraint.Object, List[(String, A)]]
    ): Dictionary[S, List[(String, A)]] = Root(schema = Reference.later(schema), validation)

    override def constraints[A](self: Dictionary[S, A]): Chain[Constraint.Object] = self.constraints

    override def schema[A](self: Dictionary[S, A]): Reference[S, ?] = self.schema
