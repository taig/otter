package io.taig.otter

import cats.Eval
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.operation.FieldOperation

sealed abstract class Field[+S[_], A] extends Product with Serializable:
  def name: String

  def schema: Reference[S, ?]

  def isOptional: Boolean

  final def imap[T](f: A => T)(g: T => A): Field[S, T] = Field.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A]

  final def optional: Field[S, Option[A]] = Field.Optional(self = this)

  final def optional(default: => A): Field[S, A] =
    Field.Default(self = this, default = Eval.later(default))

object Field:
  final case class Default[S[_], A](self: Field[S, A], default: Eval[A]) extends Field[S, A]:
    export self.{name, schema}

    override def isOptional: Boolean = true

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A] =
      copy(self = self.mapK[S1, T](fK))

  final case class Modify[S[_], A, B](self: Field[S, A], f: A => B, g: B => A) extends Field[S, B]:
    export self.{isOptional, name, schema}

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](self: Field[S, A]) extends Field[S, Option[A]]:
    export self.{name, schema}

    override def isOptional: Boolean = true

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, Option[A]] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Field[S, A]:
    override def isOptional: Boolean = false

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Field[S, *]] with
    override def imap[A, B](fa: Field[S, A])(f: A => B)(g: B => A): Field[S, B] = fa.imap(f)(g)

  given operation[S[_]]: FieldOperation[Field[S, *], S] with
    override def apply[A](name: String, value: => S[A]): Field[S, A] = Root(name, Reference.later(value))

    override def optional[A](self: Field[S, A]): Field[S, Option[A]] = self.optional

    override def optional[A](self: Field[S, A], default: => A): Field[S, A] = self.optional(default)

    override def name[A](self: Field[S, A]): String = self.name

    override def schema[A](self: Field[S, A]): Reference[S, ?] = self.schema

    override def isOptional[A](self: Field[S, A]): Boolean = self.isOptional
