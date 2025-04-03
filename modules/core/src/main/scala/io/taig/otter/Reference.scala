package io.taig.otter

import cats.Eval
import cats.~>

final case class Reference[+S[_], A](self: Eval[S[A]]) extends AnyVal:
  def value: S[A] = self.value

  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Reference[T, A] = copy(self = self.map(fK.apply))

object Reference:
  final case class Constant[+S[_], A](self: Reference[S, A], value: A):
    def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Reference.Constant[T, A] = copy(self = self.mapK(fK))

  def later[S[_], A](sa: => S[A]): Reference[S, A] = Reference(Eval.later(sa))

  def now[S[_], A](sa: S[A]): Reference[S, A] = Reference(Eval.now(sa))
