package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.RecordOperation

import scala.annotation.targetName

trait RecordComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_]](using F: RecordOperation[F, G]):
  /** The empty record. */
  val RNil: F1[Unit] = F.empty

object RecordComponent:
  trait Field[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
      F: FieldOperation[F, G]
  ):
    @targetName("applyRoundTrip")
    def apply[A](name: String, schema: => G1[A]): F1[A] = apply[A, A](name, schema)

    def apply[W, R](name: String, schema: => G[W, R]): F[W, R] = F.lift(name, Reference.later(schema))
