package io.taig.otter

import cats.Eval
import io.taig.otter.operation.FieldOperation
import cats.Invariant
import cats.derived.*

sealed abstract class Field[+S[_], A] extends Product with Serializable:
  def name: String

  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Field[S, T] = Field.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A]

  final def optional: Field[S, Option[A]] = Field.Optional(self = this)

  final def optional(default: => A): Field[S, A] =
    Field.Default(self = this, default = Eval.later(default))

object Field:
  final case class Default[S[_], A](self: Field[S, A], default: Eval[A]) extends Field[S, A]:
    export self.{name, schema}
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A] =
      copy(self = self.mapK[S1, T](fK))

  final case class Modify[S[_], A, B](self: Field[S, A], f: A => B, g: B => A) extends Field[S, B]:
    export self.{name, schema}
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](self: Field[S, A]) extends Field[S, Option[A]]:
    export self.{name, schema}
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, Option[A]] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Field[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Field[S, *]] with
    override def imap[A, B](fa: Field[S, A])(f: A => B)(g: B => A): Field[S, B] = fa.imap(f)(g)

  given operation[S[_]]: FieldOperation[S, Field] with
    override def apply[Value[a] <: S[a], A](name: String, value: => Value[A]): Field[Value, A] =
      Field.Root(name, schema = Reference.later(value))

    override def optional[Value[a] <: S[a], A](self: Field[Value, A]): Field[Value, Option[A]] = Field.Optional(self)

    override def optional[Value[a] <: S[a], A](self: Field[Value, A], default: => A): Field[Value, A] =
      Field.Default(self, Eval.later(default))
