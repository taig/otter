package io.taig.otter.schema

trait EnrichedDictionarySchema[Self[_], -Key[_], -Value[_]]
    extends DictionarySchema[Self, Key, Value],
      EnrichedSchema[Self]
