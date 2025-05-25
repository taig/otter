package io.taig.otter.http.schema

import io.taig.otter.schema.EnrichedSchema

trait EnrichedSchemaK[Self[+_[_], _]] extends SchemaK[Self]:
  def algebra[S[_]]: EnrichedSchema[Self[S, *]]
