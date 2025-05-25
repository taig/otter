package io.taig.otter.schema

trait EnrichedTupleSchema[Self[_], -Value[_]] extends TupleSchema[Self, Value], EnrichedSchema[Self]
