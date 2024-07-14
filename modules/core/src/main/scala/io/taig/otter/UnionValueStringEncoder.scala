package io.taig.otter

import cats.syntax.all.*

object UnionValueStringEncoder:
  def apply[A](schema: Union.Value[?, A], a: A): Option[String] = schema match
    case schema: Union.Value.Required[?, A]  => UnionValueRequiredStringEncoder(schema, a).some
    case Union.Value.Combine(_, left, right) => a.fold(ValueStringEncoder(left, _), ValueStringEncoder(right, _))
    case Union.Value.Optional(self)          => a.flatMap(UnionValueStringEncoder(self, _))
    case Union.Value.Transform(self, _, f)   => UnionValueStringEncoder(self, f(a))
