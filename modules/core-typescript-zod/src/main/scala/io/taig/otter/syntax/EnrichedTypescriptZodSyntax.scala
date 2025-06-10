package io.taig.otter.syntax

import io.taig.otter.operation.Enriched
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.TypescriptZodKeys
import io.taig.otter.Typescript

trait EnrichedTypescriptZodSyntax:
  extension [A](a: A)(using enriched: Enriched[A])
    def zod(value: Typescript[Typescript.Value]): A = a.metadata(TypescriptZodKeys.zod, value)
    def zod(value: String): A = zod(Typescript.Dynamic(value))

object EnrichedTypescriptZodSyntax extends EnrichedTypescriptZodSyntax
