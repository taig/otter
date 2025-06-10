package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*
import cats.Show
import cats.data.Chain
import cats.data.Chain.==:
import io.taig.otter.indent

object ZodPrinter:
  def print(zod: Typescript.Value): String = zod.show

  given Show[Typescript.Value] = _.self.show

  given Show[Typescript[Typescript.Value]] =
    case Typescript.Any                                => "z.any()"
    case Typescript.Array(self)                        => show"z.array($self)"
    case Typescript.Boolean                            => "z.boolean()"
    case Typescript.Dynamic(value)                     => value
    case Typescript.Enumeration(values)                => show"z.enumeration(TODO)"
    case Typescript.Literal(value)                     => show"z.literal($value)"
    case Typescript.Nullable(self)                     => show"z.nullable($self)"
    case Typescript.Number                             => "z.number()"
    case Typescript.Object(Chain.nil)                  => "z.object({})"
    case Typescript.Object((key, value) ==: Chain.nil) => show"z.object({ $key: $value })"
    case Typescript.Object(self) =>
      self.map((key, value) => show""""$key": $value""").map(indent(_)).mkString_("z.object({\n", ",\n", "\n})")
    case Typescript.Record(key, value)                  => show"z.record($key, $value)"
    case Typescript.Recursive(self)                     => show"z.lazy(() => $self)"
    case Typescript.Reference(name)                     => name
    case Typescript.String                              => "z.string()"
    case Typescript.Tuple(values)                       => values.mkString_("z.tuple([", ", ", "])")
    case Typescript.Union(values) if values.length == 1 => values.head.show
    case Typescript.Union(values)                       => values.mkString_("z.union([", ", ", "])")
    case Typescript.Void                                => "z.void()"
