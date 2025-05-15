package io.taig.otter

import cats.Eval

final case class Reference[+S[_], A](self: Eval[S[A]]) extends AnyVal:
  def value: S[A] = self.value

  def mapF[S1[a] >: S[a], B](f: S1[A] => S1[B]): Reference[S1, B] = copy(self = self.map(f))

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Reference[T, A] = copy(self = self.map(fK.apply))

object Reference:
  final case class Constant[+S[_], A](self: Reference[S, A], value: A):
    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Reference.Constant[T, A] =
      copy(self = self.mapK[S1, T](fK))

  def later[S[_], A](sa: => S[A]): Reference[S, A] = Reference(Eval.later(sa))

  def now[S[_], A](sa: S[A]): Reference[S, A] = Reference(Eval.now(sa))
