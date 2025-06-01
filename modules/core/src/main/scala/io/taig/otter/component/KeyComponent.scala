package io.taig.otter.component

import io.taig.otter.Key

trait KeyComponent
    extends ConstantComponent.Primitive.String[Key.Constant, Key.Primitive.String, Key.Primitive.String],
      EnumerationComponent[Key.Enumeration, Key.Primitive.String],
      PrimitiveComponent[Key.Primitive, Key.Primitive.String],
      PrimitiveComponent.Boolean[Key.Primitive.Boolean],
      PrimitiveComponent.Number[Key.Primitive.Number],
      PrimitiveComponent.String[Key.Primitive.String, Key.Primitive],
      UnionComponent[Key.Union, Key]

object KeyComponent extends KeyComponent
