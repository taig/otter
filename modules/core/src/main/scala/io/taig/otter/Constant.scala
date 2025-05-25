package io.taig.otter

import cats.Eq
import cats.syntax.all.*
import io.taig.otter.schema.ConstantSchema

sealed abstract class Constant[+S[_], A] extends Product with Serializable:
  def schema: Reference.Constant[S, ?]

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, A]
  final def imap[B](f: A => B)(g: B => A): Constant[S, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[S[_], A, B](self: Constant[S, A], f: A => B, g: B => A) extends Constant[S, B]:
    export self.schema
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](
      schema: Reference.Constant[S, A],
      eq: Eq[A]
  ) extends Constant[S, Unit]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, Unit] =
      copy(schema = schema.mapK[S1, T](fK))

  given [Value[_]]: ConstantSchema[Constant[Value, *], Value] with
    override def apply[A](schema: => Value[A], value: A)(using eq: Eq[A]): Constant[Value, Unit] =
      Root(schema = Reference.Constant(self = Reference.later(schema), value), eq)

    override def imap[A, B](fa: Constant[Value, A])(f: A => B)(g: B => A): Constant[Value, B] = fa.imap(f)(g)
