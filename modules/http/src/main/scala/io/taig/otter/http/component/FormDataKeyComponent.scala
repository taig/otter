package io.taig.otter.http.component

import io.taig.otter.component.PrimitiveComponent
import io.taig.otter.http.FormData
import io.taig.otter.component.EnumerationComponent
import io.taig.otter.component.ConstantComponent
import io.taig.otter.component.UnionComponent

trait FormDataKeyComponent
    extends ConstantComponent.Primitive.String[FormData.Key.Constant, FormData.Key.Primitive],
      EnumerationComponent[FormData.Key.Enumeration, FormData.Key.Primitive],
      PrimitiveComponent.String[FormData.Key.Primitive],
      UnionComponent[FormData.Key.Union, FormData.Key]
