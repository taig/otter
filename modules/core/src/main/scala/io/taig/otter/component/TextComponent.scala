package io.taig.otter.component

import io.taig.otter.shape.TextShape.Text

trait TextComponent
    extends BooleanComponent[Text.Boolean],
    // ConstantComponent[Text, Text.Constant.Of],
    NumberComponent[Text.Number],
    StringComponent[Text.String]

object TextComponent extends TextComponent
