package io.taig.otter

import cats.Eval

/** A lazily evaluated child schema. Suspending every child position is what makes recursive schemas expressible. */
final case class Reference[+F[-_, +_], -W, +R](self: Eval[F[W, R]]) extends AnyVal:
  def value: F[W, R] = self.value

object Reference:
  def later[F[-_, +_], W, R](fa: => F[W, R]): Reference[F, W, R] = Reference(Eval.later(fa))

  def now[F[-_, +_], W, R](fa: F[W, R]): Reference[F, W, R] = Reference(Eval.now(fa))
