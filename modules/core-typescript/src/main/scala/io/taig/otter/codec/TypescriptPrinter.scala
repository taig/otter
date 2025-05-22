package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*

object TypescriptPrinter:
  def print(typescript: Typescript): String = typescript match
    case Typescript.Any            => "any"
    case Typescript.Array(self)    => s"Array<${print(typescript = self)}>"
    case Typescript.Boolean        => "boolean"
    case Typescript.Literal(value) => value
    case Typescript.Nullable(self) => s"${print(typescript)} | null"
    case Typescript.Number         => "number"
    case Typescript.Object(self) =>
      self.map((key, value) => s"$key: ${print(value)}").mkString_("{", ", ", "}")
    case Typescript.Record(key, value) => s"Record<$key, $value>"
    case Typescript.Reference(name)    => name
    case Typescript.String             => "string"
    case Typescript.Union(left, right) => s"${print(left)} | ${print(right)}"
