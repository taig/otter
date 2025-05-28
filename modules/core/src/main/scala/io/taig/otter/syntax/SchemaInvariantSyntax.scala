package io.taig.otter.syntax

import io.taig.otter.operation.Enriched
import io.taig.otter.operation.SchemaInvariant

trait SchemaInvariantSyntax:
  implicit def schemaInvariantToEnrichedOps[S[_], A](sa: S[A])(using schema: SchemaInvariant[S]): Enriched.Ops[S[A]] =
    new Enriched.Ops(sa)(using schema.enriched[A])

object SchemaInvariantSyntax extends SchemaInvariantSyntax
