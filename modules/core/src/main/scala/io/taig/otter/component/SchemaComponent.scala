package io.taig.otter.component

import io.taig.otter.shape.SchemaShape.*

object SchemaComponent
    extends CollectionComponent[Schema.Collection.Of, Schema],
      DictionaryComponent[Schema.Dictionary.Of, Schema]
