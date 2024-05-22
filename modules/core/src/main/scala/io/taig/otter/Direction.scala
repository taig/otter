package io.taig.otter

sealed trait Isomorphic[F[_], A]: // extends Reader[A, B], Writer[A, B]
  def map[C](f: F[A] => F[C]): Isomorphic[F, C]

object Isomorphic:
  final case class Root[F[_], A](fa: F[A]) extends Isomorphic[F, A]:
    override def map[C](f: F[A] => F[C]): Isomorphic[F, C] = copy(fa = f(fa))

// sealed trait Reader[A, +B] extends Product, Serializable

// sealed trait Writer[A, -B] extends Product, Serializable
