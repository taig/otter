package io.taig.otter.syntax

import io.taig.otter.Keys
import io.taig.otter.operation.Enriched
import io.taig.otter.Metadata
import cats.syntax.all.*

trait EnrichedSyntax:
  extension [A](a: A)(using enriched: Enriched[A])
    def metadata: Metadata = enriched.metadata(a)
    def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)

    def metadata(f: Metadata => Metadata): A = enriched.modifyMetadata(a)(f)
    def metadata[B](key: Metadata.Key[B], f: Option[B] => Option[B]): A = metadata: metadata =>
      f(metadata.get(key)).fold(metadata.remove(key))(metadata.put(key, _))
    def metadata[B](key: Metadata.Key[B], value: Option[B]): A = metadata(key, _ => value)
    def metadata[B](key: Metadata.Key[B], value: B): A = metadata(key, value.some)

    def description(value: String): A = metadata(Keys.description, value)
    def hidden(value: Boolean): A = metadata(Keys.hidden, value)
    def hidden: A = hidden(true)
    def name(value: String): A = metadata(Keys.name, value)
    def namespace(value: String): A = metadata(Keys.namespace, value)

object EnrichedSyntax extends EnrichedSyntax
