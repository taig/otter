package io.taig.otter

import cats.Eval

final case class Reference[+S[_], A](self: Eval[S[A]]) extends AnyVal:
  def value: S[A] = self.value

object Reference:
  def later[S[_], A](sa: => S[A]): Reference[S, A] = Reference(Eval.later(sa))

  def now[S[_], A](sa: S[A]): Reference[S, A] = Reference(Eval.now(sa))
