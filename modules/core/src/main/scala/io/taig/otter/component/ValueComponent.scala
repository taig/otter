package io.taig.otter.component

import io.taig.otter.Value

trait ValueComponent
    extends ConstantComponent[Value.Constant, Value.Primitive.Text],
      PrimitiveComponent.Boolean[Value.Primitive.Boolean],
      PrimitiveComponent.Coerce.Text[Value.Primitive.Coerce, [a] =>> Value.Primitive.Boolean[a] | Value.Primitive.Number[a] | Value.Primitive.Text[a]],
      PrimitiveComponent.Number[Value.Primitive.Number],
      PrimitiveComponent.Text[Value.Primitive.Text]

object ValueComponent extends ValueComponent