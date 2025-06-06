package io.taig.otter.syntax

import io.taig.otter.operation.Enriched
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.TypescriptZodKeys

trait EnrichedTypescriptZodSyntax:
  extension [A](a: A)(using enriched: Enriched[A]) def zod(value: String): A = a.metadata(TypescriptZodKeys.zod, value)

object EnrichedTypescriptZodSyntax extends EnrichedTypescriptZodSyntax
