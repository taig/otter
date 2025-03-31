package io.taig.otter

import cats.Eval

final case class Reference[+S[_], A](self: Eval[S[A]]) extends AnyVal:
  def value: S[A] = self.value
