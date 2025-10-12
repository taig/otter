package io.taig.otter.component

import io.taig.otter.shape.SchemaShape.*

trait SchemaComponent
    extends BooleanComponent[Schema.Primitive.Boolean],
      CoerceComponent[Schema, Schema.Coerce.Of],
      ConstantComponent[Schema, Schema.Constant.Of],
      DictionaryComponent[Schema, Schema.Dictionary.Of],
      EnumerationComponent[Schema, Schema.Enumeration.Of],
      NumberComponent[Schema.Primitive.Number],
      NullableComponent[Schema, Schema.Nullable.Of],
      PrimitiveComponent[Schema.Primitive],
      RecordComponent[Schema, Schema.Record.Of],
      StringComponent[Schema.Primitive.String],
      TupleComponent[Schema, Schema.Tuple.Of],
      UnionComponent[Schema, Schema.Union.Of]

object SchemaComponent extends SchemaComponent
