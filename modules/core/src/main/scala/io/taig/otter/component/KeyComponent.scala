package io.taig.otter.component

import io.taig.otter.Key

trait KeyComponent
    extends ConstantComponent.Primitive.String[Key.Constant, Key.Primitive],
      EnumerationComponent[Key.Enumeration, Key.Primitive],
      PrimitiveComponent.String[Key.Primitive],
      UnionComponent[Key.Union, Key]

object KeyComponent extends KeyComponent
