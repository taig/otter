package io.taig.otter

import cats.derived.*
import cats.syntax.all.*
import cats.data.Chain.==:
import cats.Show
import cats.Order
import cats.data.NonEmptyChain
import cats.data.Chain

enum Zod derives Order:
  case Array(self: Zod)
  case Expression(value: String)
  case Lazy(self: Zod)
  case Literal(value: String)
  case Nullable(self: Zod)
  case Record(key: Zod, value: Zod)
  case Reference(name: String)
  case Object(fields: Chain[(String, Zod)])
  case Tuple(values: Chain[Zod])
  case Union(values: NonEmptyChain[Zod])

  final override def toString: String = this.show

object Zod:
  given Show[Zod] =
    case Zod.Array(self)                        => show"z.array($self)"
    case Zod.Expression(value)                  => value
    case Zod.Lazy(self)                         => show"z.lazy(() => $self)"
    case Zod.Literal(value)                     => show"z.literal($value)"
    case Zod.Nullable(self)                     => show"z.nullable($self)"
    case Zod.Object(Chain.nil)                  => "z.object({})"
    case Zod.Object((key, value) ==: Chain.nil) => show"z.object({ $key: $value })"
    case Zod.Object(self) =>
      self.map((key, value) => show""""$key": $value""").map(indent(_)).mkString_("z.object({\n", "\n", "\n})")
    case Zod.Record(key, value) => show"z.record($key, $value)"
    case Zod.Reference(name)    => name
    case Zod.Tuple(values)      => values.mkString_("z.tuple([", ", ", "])")
    case Zod.Union(values)      => values.mkString_("z.union([", ", ", "])")
