package io.taig.otter.syntax

import io.taig.otter.Keys
import io.taig.otter.operation.SchemaInvariant

trait MetadataSyntax:
  extension [S[_]: SchemaInvariant, A](self: S[A])
    def description: Option[String] = self.metadata(Keys.description)
    def description(value: String): S[A] = self.metadata(Keys.description, value)

    def name: Option[String] = self.metadata(Keys.name)
    def name(value: String): S[A] = self.metadata(Keys.name, value)

    def namespace: Option[String] = self.metadata(Keys.namespace)
    def namespace(value: String): S[A] = self.metadata(Keys.namespace, value)
