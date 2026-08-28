package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.CoerceOperation

import scala.annotation.targetName

trait CoerceComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
    F: CoerceOperation[F, G]
):
  @targetName("applyRoundTrip")
  def apply[A](schema: => G1[A]): F1[A] = apply[A, A](schema)

  def apply[W, R](schema: => G[W, R]): F[W, R] = F.lift(Reference.later(schema))
