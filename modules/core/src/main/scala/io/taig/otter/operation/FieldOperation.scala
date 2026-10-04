package io.taig.otter.operation

import io.taig.otter.Field
import io.taig.otter.Reference

/** Constructs the field type `F` of a record whose schemas have type `G`. */
trait FieldOperation[F[-_, +_], G[-_, +_]]:
  def lift[W, R](name: String, schema: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R])
    def name: String
    def isOptional: Boolean

    /** A missing name reads as None; None omits the name when written. */
    def optional: F[Option[W], Option[R]]

    /** Selects one complete absence contract; a second contract on this field is a construction error. */
    def optional(presence: Field.Presence): F[Option[W], Option[R]]

    /** A missing name reads the lazy default; writes always use the supplied payload value. */
    def defaulted(default: => R): F[W, R]

    /** Selects the wire forms that trigger a lazy read-side default. */
    def defaulted(default: => R, absent: Field.Absent): F[W, R]
    def schema: Reference[G, ?, ?]

object FieldOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: FieldOperation[F, G]): FieldOperation[F, G] = self
