package io.taig.otter

import cats.~>
import cats.arrow.FunctionK

sealed trait Isomorphic[F[_], A]: // extends Reader[A, B], Writer[A, B]
  def mapF[B](f: F[A] => F[B]): Isomorphic[F, B]
  def mapK[G[_]](fK: F ~> G): Isomorphic[G, A]

  final def imap[B](f: A => B)(g: B => A): Isomorphic[F, B] = Isomorphic.Modify(this, f, g)

object Isomorphic:
  final case class Root[F[_], A](fa: F[A]) extends Isomorphic[F, A]:
    override def mapF[B](f: F[A] => F[B]): Isomorphic[F, B] = copy(fa = f(fa))
    override def mapK[G[_]](fK: FunctionK[F, G]): Isomorphic[G, A] = copy(fK.apply(fa))

  final case class Modify[F[_], A, B](fa: Isomorphic[F, A], f: A => B, g: B => A) extends Isomorphic[F, B]:
    override def mapF[C](f: F[B] => F[C]): Isomorphic[F, C] = ???
    override def mapK[G[_]](fK: FunctionK[F, G]): Isomorphic[G, B] = copy(fa = fa.mapK(fK))

// sealed trait Reader[A, +B] extends Product, Serializable

// sealed trait Writer[A, -B] extends Product, Serializable
