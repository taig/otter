package io.taig.otter.schema

trait EnrichedEnumerationSchema[Self[_], Value[_]] extends EnumerationSchema[Self, Value], EnrichedSchema[Self]
