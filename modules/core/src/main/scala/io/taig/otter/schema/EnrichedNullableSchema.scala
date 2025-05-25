package io.taig.otter.schema

trait EnrichedNullableSchema[Self[_], Value[_]] extends NullableSchema[Self, Value], EnrichedSchema[Self]
