package io.taig.otter.schema

trait EnrichedFieldSchema[Self[_], Key[_], Value[_]] extends FieldSchema[Self, Key, Value], EnrichedSchema[Self]
