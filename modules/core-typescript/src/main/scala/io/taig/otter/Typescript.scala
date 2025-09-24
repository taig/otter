package io.taig.otter

import cats.Order
import cats.Show
import cats.Traverse
import cats.data.Chain
import cats.data.Chain.==:
import cats.data.NonEmptyChain
import cats.derived
import cats.derived.*
import cats.syntax.all.*

enum Typescript[+A] derives Order, Traverse:
  case Array(self: A)
  case Boolean extends Typescript[Nothing]
  case Dynamic(value: String) extends Typescript[Nothing]
  case Literal(value: String) extends Typescript[Nothing]
  case Nullable(self: A)
  case Number extends Typescript[Nothing]
  case Object(fields: Chain[(String, A)])
  case Record(key: A, value: A)
  case Reference(name: String) extends Typescript[Nothing]
  case String extends Typescript[Nothing]
  case Tuple(values: Chain[A])
  case Union(values: NonEmptyChain[A])
  case Void extends Typescript[Nothing]

object Typescript:
  final case class Value(self: Typescript[Typescript.Value]) extends AnyVal

  object Value:
    given Order[Typescript.Value] with
      override def compare(x: Typescript.Value, y: Typescript.Value): Int = x.self.compare(y.self)

    given Show[Typescript.Value] = _.self.show

  given [A: Show]: Show[Typescript[A]] =
    case Typescript.Array(self)                        => show"ReadonlyArray<$self>"
    case Typescript.Boolean                            => "boolean"
    case Typescript.Dynamic(value)                     => value
    case Typescript.Literal(value)                     => value
    case Typescript.Nullable(self)                     => show"($self | null)"
    case Typescript.Number                             => "number"
    case Typescript.Object(Chain.nil)                  => "{}"
    case Typescript.Object((key, value) ==: Chain.nil) => show"{ $key: $value }"
    case Typescript.Object(self)                       =>
      self.map((key, value) => show""""$key": $value""").map(indent(_)).mkString_("{\n", "\n", "\n}")
    case Typescript.Record(key, value) => show"{ [key: $key]: $value }"
    case Typescript.Reference(name)    => name
    case Typescript.String             => "string"
    case Typescript.Tuple(values)      => values.mkString_("[", ", ", "]")
    case Typescript.Union(values)      => values.mkString_(" | ")
    case Typescript.Void               => "void"
