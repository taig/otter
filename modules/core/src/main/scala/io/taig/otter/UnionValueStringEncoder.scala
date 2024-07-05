package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.Plain.*
import cats.syntax.all.*

object UnionValueStringEncoder:
  def apply[A](schema: Union.Value.Writer[A], a: A): Option[String] = schema match
    case schema: Union.Value.Required.Writer[A]       => UnionValueRequiredStringEncoder(schema, a).some
    case Base.Union.Value.Combine(left, right)        => combine(left, right, a)
    case Base.Union.Value.Optional(self)              => optional(self, a)
    case Base.Union.Value.Transform(self, _, f)       => transform(self, f, a)
    case Base.Union.Value.Writer.Combine(left, right) => combine(left, right, a)
    case Base.Union.Value.Writer.Optional(self)       => optional(self, a)
    case Base.Union.Value.Writer.Transform(self, f)   => transform(self, f, a)

  def combine[A, B](
      left: Union.Value.Writer[A],
      right: Union.Value.Writer[B],
      ab: Either[A, B]
  ): Option[String] = ab.fold(ValueStringEncoder(left, _), ValueStringEncoder(right, _))

  def optional[A](self: Union.Value.Writer[A], a: Option[A]): Option[String] =
    a.flatMap(UnionValueStringEncoder(self, _))

  def transform[A, B](self: Union.Value.Writer[A], f: B => A, b: B): Option[String] =
    UnionValueStringEncoder(self, f(b))
