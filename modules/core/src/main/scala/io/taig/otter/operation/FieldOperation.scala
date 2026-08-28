package io.taig.otter.operation

import io.taig.otter.Reference

/** Constructs the field type `F` of a record whose schemas have type `G`. */
trait FieldOperation[F[-_, +_], G[-_, +_]]:
  def lift[W, R](name: String, schema: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R])
    def name: String
    def isOptional: Boolean
    def optional: F[Option[W], Option[R]]
    def optional(default: => R): F[W, R]
    def schema: Reference[G, ?, ?]

object FieldOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: FieldOperation[F, G]): FieldOperation[F, G] = self
