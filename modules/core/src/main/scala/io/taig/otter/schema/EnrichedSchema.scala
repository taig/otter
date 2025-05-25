package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Metadata.*

trait EnrichedSchema[Self[_]] extends Schema[Self]:
  extension [A](self: Self[A])
    def metadata: Metadata
    def metadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: Option[B]): Self[A] =
      metadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = metadata(_.put(key, value))

object EnrichedSchema:
  inline def apply[Self[_]](using self: EnrichedSchema[Self]): EnrichedSchema[Self] = self
