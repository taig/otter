package io.taig.otter.component

import io.taig.otter.Key

trait KeyComponent
    extends ConstantComponent.Primitive.String[Key.Constant, Key],
      EnumerationComponent[Key.Enumeration, Key],
      PrimitiveComponent.String[Key.Primitive],
      UnionComponent[Key.Union, Key]

object KeyComponent extends KeyComponent
