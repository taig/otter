package io.taig.otter.schema

trait EnrichedRecordSchema[Self[_], -Field[_]] extends RecordSchema[Self, Field], EnrichedSchema[Self]
