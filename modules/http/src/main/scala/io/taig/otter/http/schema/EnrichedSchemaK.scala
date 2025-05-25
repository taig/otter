package io.taig.otter.http.schema

import io.taig.otter.Metadata

trait EnrichedSchemaK[Self[_[_], _]] extends SchemaK[Self]:
  extension [S[_], A](self: Self[S, A])
    def metadata: Metadata

object EnrichedSchemaK