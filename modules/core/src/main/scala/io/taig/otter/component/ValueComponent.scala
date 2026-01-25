package io.taig.otter.component

import io.taig.otter.Value

trait ValueComponent
    extends CoerceComponent[Value.Coerce, Value.Primitive],
      ConstantComponent[Value.Constant, Value.Primitive.Text],
      PrimitiveComponent.Boolean[Value.Primitive.Boolean],
      PrimitiveComponent.Number[Value.Primitive.Number],
      PrimitiveComponent.Text[Value.Primitive.Text]

object ValueComponent extends ValueComponent
