package io.taig.otter.component

import io.taig.otter.shape.TextShape.Text

trait TextComponent
    extends BooleanComponent[Text.Primitive.Boolean],
      CoerceComponent[Text.Primitive, Text.Coerce.Of],
      ConstantComponent[Text, Text.Constant.Of],
      EnumerationComponent[Text, Text.Enumeration.Of],
      NumberComponent[Text.Primitive.Number],
      PrimitiveComponent[Text.Primitive],
      StringComponent[Text.Primitive.String],
      UnionComponent[Text, Text.Union.Of]

object TextComponent extends TextComponent
