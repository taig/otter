package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.RecordOperation

trait RecordComponent[F[-_, +_], G[-_, +_]](using F: RecordOperation[F, G]):
  /** The empty record. */
  val RNil: F[Unit, Unit] = F.empty

object RecordComponent:
  trait Field[F[-_, +_], G[-_, +_]](using F: FieldOperation[F, G]):
    def apply[W, R](name: String, schema: => G[W, R]): F[W, R] = F.lift(name, Reference.later(schema))
