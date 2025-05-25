package io.taig.otter.schema

trait EnrichedUnionSchema[Self[_], Value[_]] extends UnionSchema[Self, Value], EnrichedSchema[Self]
