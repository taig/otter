package io.taig.otter.schema

trait EnrichedPrimitiveSchema[Self[_]] extends PrimitiveSchema[Self], EnrichedSchema[Self]
