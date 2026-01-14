package io.taig.otter

import cats.Eval

final case class Reference[+F[_], A](self: Eval[F[A]]) extends AnyVal:
  def value: F[A] = self.value

  def map[F1[a] >: F[a], B](f: F1[A] => F1[B]): Reference[F1, B] = copy(self = self.map(f))

  def mapK[F1[a] >: F[a], G[_]](fK: [A] => F1[A] => G[A]): Reference[G, A] = copy(self = self.map(fK.apply))

object Reference:
  def later[F[_], A](sa: => F[A]): Reference[F, A] = Reference(Eval.later(sa))

  def now[F[_], A](sa: F[A]): Reference[F, A] = Reference(Eval.now(sa))
