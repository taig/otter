package io.taig.otter

import cats.data.Chain
import cats.data.Chain.==:
import cats.Show
import cats.syntax.all.*
import cats.derived.*
import cats.Order

enum Typescript derives Order:
  case Any
  case Array(self: Typescript)
  case Boolean
  case Literal(value: String)
  case Nullable(self: Typescript)
  case Number
  case Object(fields: Chain[(String, Typescript)])
  case Record(key: Typescript, value: Typescript)
  case Reference(name: String)
  case String
  case Tuple(values: Chain[Typescript])
  case Union(left: Typescript, right: Typescript)
  case Void

  final def definition(name: String): TypescriptDefinition =
    TypescriptDefinition(name, value = this)

  final override def toString: String = this.show

object Typescript:
  given Show[Typescript] =
    case Typescript.Any                                => "any"
    case Typescript.Array(self)                        => show"Array<$self>"
    case Typescript.Boolean                            => "boolean"
    case Typescript.Literal(value)                     => value
    case Typescript.Nullable(self)                     => show"($self | null)"
    case Typescript.Number                             => "number"
    case Typescript.Object(Chain.nil)                  => "{}"
    case Typescript.Object((key, value) ==: Chain.nil) => show"{ $key: $value }"
    case Typescript.Object(self) =>
      self.map((key, value) => show"$key: $value").map(indent(_)).mkString_("{\n", "\n", "\n}")
    case Typescript.Record(key, value) => show"{ [key: $key]: $value }"
    case Typescript.Reference(name)    => name
    case Typescript.String             => "string"
    case Typescript.Tuple(values)      => values.mkString_("[", ", ", "]")
    case Typescript.Union(left, right) => show"$left | $right"
    case Typescript.Void               => "void"
