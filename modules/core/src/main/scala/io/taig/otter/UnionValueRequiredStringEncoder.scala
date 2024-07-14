package io.taig.otter

import io.taig.otter as Base

object UnionValueRequiredStringEncoder:
  def apply[A](schema: Union.Value.Required[?, A], a: A): String = schema match
    case Base.Union.Value.Required.Combine(_, left, right) =>
      a.fold(ValueRequiredStringEncoder(left, _), ValueRequiredStringEncoder(right, _))
    case Base.Union.Value.Required.Transform(self, _, f) => UnionValueRequiredStringEncoder(self, f(a))
