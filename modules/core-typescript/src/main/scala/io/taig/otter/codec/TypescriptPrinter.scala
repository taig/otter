package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*
import cats.data.Chain
import cats.data.Chain.==:
import cats.Show
import io.taig.otter.indent

object TypescriptPrinter:
  def print(typescript: Typescript.Value): String = typescript.show

  given Show[Typescript.Value] = _.self.show

  given Show[Typescript[Typescript.Value]] =
    case Typescript.Any                                => "any"
    case Typescript.Array(self)                        => show"Array<$self>"
    case Typescript.Boolean                            => "boolean"
    case Typescript.Dynamic(value)                     => value
    case Typescript.Enumeration(values)                => values.mkString_(" | ")
    case Typescript.Literal(value)                     => value
    case Typescript.Nullable(self)                     => show"($self | null)"
    case Typescript.Number                             => "number"
    case Typescript.Object(Chain.nil)                  => "{}"
    case Typescript.Object((key, value) ==: Chain.nil) => show"{ $key: $value }"
    case Typescript.Object(self) =>
      self.map((key, value) => show""""$key": $value""").map(indent(_)).mkString_("{\n", "\n", "\n}")
    case Typescript.Record(key, value) => show"{ [key: $key]: $value }"
    case Typescript.Recursive(self)    => self.show
    case Typescript.Reference(name)    => name
    case Typescript.String             => "string"
    case Typescript.Tuple(values)      => values.mkString_("[", ", ", "]")
    case Typescript.Union(values)      => values.mkString_(" | ")
    case Typescript.Void               => "void"
