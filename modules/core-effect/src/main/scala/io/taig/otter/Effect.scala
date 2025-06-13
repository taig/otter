package io.taig.otter

import cats.Order
import cats.Show
import cats.Traverse
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.derived
import cats.derived.*
import cats.syntax.all.*

enum Effect[+A] derives Order, Traverse:
  case Array(self: A)
  case Boolean extends Effect[Nothing]
  case Dynamic(value: String) extends Effect[Nothing]
  case Literal(value: String) extends Effect[Nothing]
  case Nullable(self: A)
  case Number extends Effect[Nothing]
  case Recursion(tpe: String, self: A) extends Effect[A]
  case Record(key: A, value: A)
  case Reference(name: String) extends Effect[Nothing]
  case String extends Effect[Nothing]
  case Struct(fields: Chain[(String, A)])
  case Tuple(values: Chain[A])
  case Union(values: NonEmptyChain[A])
  case Void extends Effect[Nothing]

  def isRecursion: Boolean = this match
    case Recursion(_, _) => true
    case _               => false

object Effect:
  final case class Value(self: Effect[Effect.Value]) extends AnyVal

  object Value:
    given Order[Effect.Value] with
      override def compare(x: Effect.Value, y: Effect.Value): Int = x.self.compare(y.self)

    given Show[Effect.Value] = _.self.show

  given [A: Show]: Show[Effect[A]] =
    case Array(self)        => show"Schema.Array($self)"
    case Boolean            => "Schema.Boolean"
    case Dynamic(value)     => value
    case Literal(value)     => show"Schema.Literal(${value.show})"
    case Nullable(self)     => show"Schema.NullOr($self)"
    case Record(key, value) => show"Schema.Record({ key: $key, value: $value })"
    case Reference(name)    => name
    case String             => "Schema.String"
    case Struct(fields) =>
      show"Schema.Struct({ ${fields.map((name, value) => show"$name: $value").mkString_(", ")} })"
    case Number                               => "Schema.Number"
    case Recursion(tpe, self)                 => show"Schema.suspend((): Schema.Schema<$tpe> => $self)"
    case Union(values) if values.length === 1 => values.head.show
    case Union(values)                        => show"Schema.Union(${values.map(_.show).mkString_(", ")})"
    case Tuple(values)                        => show"Schema.Tuple(${values.map(_.show).mkString_(", ")})"
    case Void                                 => "Schema.Void"
