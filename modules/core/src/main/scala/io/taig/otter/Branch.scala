package io.taig.otter

import cats.Invariant
import io.taig.otter.operation.BranchOperation

sealed abstract class Branch[+S[_], A] extends Product, Serializable:
  def name: String

  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Branch[S, T] = Branch.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Branch[T, A]

object Branch:
  final case class Modify[S[_], A, B](self: Branch[S, A], f: A => B, g: B => A) extends Branch[S, B]:
    export self.{name, schema}

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Branch[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Branch[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Branch[T, A] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Branch[S, *]] with
    override def imap[A, B](fa: Branch[S, A])(f: A => B)(g: B => A): Branch[S, B] =
      fa.imap(f)(g)

  given operation[S[_]]: BranchOperation[Branch[S, *], S] with
    override def apply[A](name: String, value: => S[A]): Branch[S, A] =
      Root(name, Reference.later(value))

    override def name[A](self: Branch[S, A]): String = self.name

    override def schema[A](self: Branch[S, A]): Reference[S, ?] = self.schema
