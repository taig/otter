package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.derived.*
import cats.derived
import cats.syntax.all.*
import cats.Traverse
import cats.Show

enum Effect[+A] derives Order, Traverse:
  case Boolean
  case Collection(self: A)
  case Literal(value: String)
  case Nullable(self: A)
  case Number
  case Object(fields: Chain[(String, A)])
  case Recursive(self: A)
  case Reference(name: String)
  case String
  case Tuple(values: Chain[A])
  case Union(values: NonEmptyChain[A])
  case Void

object Effect:
  final case class Value(self: Effect[Effect.Value]) extends AnyVal

  object Value:
    given Order[Effect.Value] with
      override def compare(x: Effect.Value, y: Effect.Value): Int = x.self.compare(y.self)

    given Show[Effect.Value] = _.self.show

  given [A: Show]: Show[Effect[A]] =
    case Boolean => "Schema.Boolean"
    case _       => "TODO"
