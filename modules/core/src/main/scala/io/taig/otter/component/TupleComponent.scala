package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_]](using F: TupleOperation[F, G]):
  /** The empty tuple. */
  val TNil: F1[Unit] = F.empty
