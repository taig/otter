package io.taig.otter

import cats.Eq
import io.taig.otter.operation.ConstantOperation

sealed abstract class Constant[+S[_], A]:
  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Constant[S, T] = Constant.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, A]

object Constant:
  final case class Modify[S[_], A, B](self: Constant[S, A], f: A => B, g: B => A) extends Constant[S, B]:
    export self.schema
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A], value: A, eq: Eq[A]) extends Constant[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Constant[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Constant[S, *]] with
    extension [A](self: Constant[S, A]) override def imap[B](f: A => B)(g: B => A): Constant[S, B] = self.imap(f)(g)

  // given operation[S[_]]: ConstantOperation[S, Constant] with
  //   override def constant[A](schema: => S[A], value: A)(using eq: Eq[A]): Constant[S, A] =
  //     Root(schema = Reference.later(schema), value, eq)

  // given operation2[S[_]]: ConstantOperation2[S, Constant] with
  //   override def constant[Value[a] <: S[a], A](schema: => Value[A], value: A)(using eq: Eq[A]): Constant[Value, A] =
  //     Constant.Root(schema = Reference.later(schema), value, eq)
