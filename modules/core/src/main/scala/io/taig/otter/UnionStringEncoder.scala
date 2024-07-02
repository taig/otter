package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.Plain.*

object UnionStringEncoder:
  def apply[A](schema: Union.Value.Required.Writer[A], a: A): String = schema match
    case Base.Union.Value.Required.Writer.Root(schema) => StringEncoder(schema, a)
