package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.BranchOperation

import scala.annotation.targetName

/** @tparam F1
  *   the one parameter alias of `F`, so that a round tripping branch infers as `Json.Branch[A]` rather than
  *   `Json.Branch.Of[A, A]`. The bounds make it definitionally equal to `F[A, A]`.
  */
trait BranchComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
    F: BranchOperation[F, G]
):
  @targetName("applyRoundTrip")
  def apply[A](name: String, schema: => G1[A]): F1[A] = apply[A, A](name, schema)

  def apply[W, R](name: String, schema: => G[W, R]): F[W, R] = F.lift(name, Reference.later(schema))
