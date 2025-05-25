package io.taig.otter.schema

trait EnrichedConstantSchema[Self[_], -Value[_]] extends ConstantSchema[Self, Value], EnrichedSchema[Self]
