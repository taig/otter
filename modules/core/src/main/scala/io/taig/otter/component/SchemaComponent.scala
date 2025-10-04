package io.taig.otter.component

import io.taig.otter.shape.SchemaShape.Schema

trait SchemaComponent
    extends BooleanComponent[Schema.Primitive.Boolean],
      FieldComponent[Schema, Schema.Field.Of],
      NumberComponent[Schema.Primitive.Number],
      RecordComponent[Schema, Schema.Record.Of, Schema.Field.Of],
      StringComponent[Schema.Primitive.String]

object SchemaComponent extends SchemaComponent
