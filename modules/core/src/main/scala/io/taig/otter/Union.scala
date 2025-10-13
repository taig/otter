package io.taig.otter

import cats.data.NonEmptyChain
import cats.Invariant
import cats.derived.*
import io.taig.otter.operation.UnionOperation

sealed abstract class Union[+S[_], A] extends Product with Serializable:
  def schemas: NonEmptyChain[Reference[S, ?]]

  final def imap[T](f: A => T)(g: T => A): Union[S, T] = Union.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, A]

object Union:
  final case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.schemas

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class OrElse[S[_], A, B](left: Union[S, A], right: Union[S, B]) extends Union[S, Either[A, B]]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = left.schemas ++ right.schemas

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, Either[A, B]] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A]) extends Union[S, A]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(schema)

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Union[S, *]] with
    override def imap[A, B](fa: Union[S, A])(f: A => B)(g: B => A): Union[S, B] = fa.imap(f)(g)

  given operation[S[_]]: UnionOperation[Union[S, *], S] with
    override def lift[A](value: => S[A]): Union[S, A] = Root(schema = Reference.later(value))

    override def orElse[A, B](left: Union[S, A], right: Union[S, B]): Union[S, Either[A, B]] = OrElse(left, right)
