package io.taig.otter.syntax

import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.operation.Enriched
import io.taig.otter.Zod
import io.taig.otter.ZodKeys

trait EnrichedZodSyntax:
  extension [A](a: A)(using Enriched[A]) def zod(value: Zod): A = a.metadata(ZodKeys.zod, value)

object EnrichedZodSyntax extends EnrichedZodSyntax
