package io.taig.otter.component

import io.taig.otter.Text

trait TextComponent
    extends BooleanComponent[Text.Primitive.Boolean],
      CoerceComponent[Text.Coerce, Text.Primitive],
      ConstantComponent[Text.Constant, Text],
      EnumerationComponent[Text.Enumeration, Text],
      NumberComponent[Text.Primitive.Number],
      PrimitiveComponent[Text.Primitive],
      StringComponent[Text.Primitive.String],
      UnionComponent[Text.Union, Text]

object TextComponent extends TextComponent
