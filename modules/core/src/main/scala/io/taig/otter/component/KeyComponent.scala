package io.taig.otter.component

import io.taig.otter.Key

trait KeyComponent
    extends ConstantComponent.Primitive.String[Key.Constant, Key.Primitive.String],
      EnumerationComponent[Key.Enumeration, Key.Primitive.String],
      PrimitiveComponent[Key.Primitive, Key.Primitive.String]

object KeyComponent extends KeyComponent
