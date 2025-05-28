package io.taig.otter.syntax

import io.taig.otter.operation.Enriched
import io.taig.otter.Keys
import io.taig.otter.operation.SchemaInvariant

trait EnrichedSyntax:
  implicit def enrichedToEnrichedOps[A: Enriched](a: A): Enriched.Ops[A] = new Enriched.Ops(a)

  extension [A](self: Enriched.Ops[A])
    def description: Option[String] = self.metadata(Keys.description)
    def description(value: String): A = self.metadata(Keys.description, value)

    def name: Option[String] = self.metadata(Keys.name)
    def name(value: String): A = self.metadata(Keys.name, value)

    def namespace: Option[String] = self.metadata(Keys.namespace)
    def namespace(value: String): A = self.metadata(Keys.namespace, value)

object EnrichedSyntax extends EnrichedSyntax
