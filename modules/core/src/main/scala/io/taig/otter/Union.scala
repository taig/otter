package io.taig.otter

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.schema.UnionSchema

sealed abstract class Union[+S[_], A] extends Product with Serializable:
  def schemas: NonEmptyChain[Reference[S, ?]]

  final def imap[B](f: A => B)(g: B => A): Union[S, B] = Union.Modify(self = this, f, g)

  final def orElse[S1[a] >: S[a], B](schema: Union[S1, B]): Union[S1, Either[A, B]] =
    Union.OrElse(left = this, right = schema)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, A]

object Union:
  final private[otter] case class Modify[S[_], A, B](self: Union[S, A], f: A => B, g: B => A) extends Union[S, B]:
    export self.schemas
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, B] = copy(self = self.mapK[S1, T](fK))

  final private[otter] case class OrElse[S[_], A, B](left: Union[S, A], right: Union[S, B])
      extends Union[S, Either[A, B]]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = left.schemas ++ right.schemas
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, Either[A, B]] =
      copy(left = left.mapK[S1, T](fK), right = right.mapK[S1, T](fK))

  final private[otter] case class Root[S[_], A](schema: Reference[S, A]) extends Union[S, A]:
    override def schemas: NonEmptyChain[Reference[S, ?]] = NonEmptyChain.one(schema)
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Union[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given [Value[_]]: UnionSchema[Union[Value, *], Value] with
    override def lift[A](schema: => Value[A]): Union[Value, A] = Union.Root(schema = Reference.later(schema))

    override def imap[A, B](fa: Union[Value, A])(f: A => B)(g: B => A): Union[Value, B] = fa.imap(f)(g)

    extension [A](self: Union[Value, A])
      override def schemas: NonEmptyChain[Reference[Value, ?]] = self.schemas
      override def orElse[B](schema: Union[Value, B]): Union[Value, Either[A, B]] = self.orElse(schema)
