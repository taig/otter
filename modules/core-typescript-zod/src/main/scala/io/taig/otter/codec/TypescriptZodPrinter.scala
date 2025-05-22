package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*
import io.taig.otter.zodObject
import cats.data.Chain

object TypescriptZodPrinter:
  def print(typescript: Typescript): String = typescript match
    case Typescript.Any                => "z.any()"
    case Typescript.Array(self)        => s"z.array(${print(self)})"
    case Typescript.Boolean            => "z.boolean()"
    case Typescript.Literal(value)     => s"z.literal($value)"
    case Typescript.Nullable(self)     => s"z.nullable(${print(self)})"
    case Typescript.Number             => "z.number()"
    case Typescript.Object(fields)     => zodObject(fields.map(_.map(print(_))))
    case Typescript.Record(key, value) => s"z.record(${print(key)}, ${print(value)})"
    case Typescript.Reference(name)    => name
    case Typescript.String             => s"z.string()"
    case Typescript.Tuple(values)      => s"z.tuple([${values.map(print).mkString_(", ")}])"
    case typescript: Typescript.Union => print(typescript).map(print).mkString_("z.union([", ", ", "])")
    case Typescript.Void               => s"z.void()"

  def print(typescript: Typescript.Union): Chain[Typescript] = typescript match
    case Typescript.Union(left: Typescript.Union, right: Typescript.Union) => print(left) ++ print(right)
    case Typescript.Union(left: Typescript.Union, right) => print(left) :+ right
    case Typescript.Union(left, right: Typescript.Union) => left +: print(right)
    case Typescript.Union(left, right) => Chain(left, right)
