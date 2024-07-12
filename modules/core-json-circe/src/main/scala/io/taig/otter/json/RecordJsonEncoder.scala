package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import cats.data.Chain

object RecordJsonEncoder:
  def apply[A](schema: Record.Writer.Via[Json, A], a: A): Option[Chain[(String, Json)]] =
    RecordJsonEncoder(schema, Null.Default, a)

  // TODO nulls
  def apply[A](schema: Record.Writer.Via[Json, A], nulls: Null, a: A): Option[Chain[(String, Json)]] =
    schema match
      case Record.Combine(_, left, right)        => combine(left, right, nulls, a).some
      case Record.Empty(_)                       => Chain.empty.some
      case Record.One(_, field)                  => one(field, nulls, a)
      case Record.Optional(self)                 => optional(self, nulls, a)
      case Record.Transform(self, _, f)          => transform(self, f, nulls, a)
      case Record.Writer.Combine(_, left, right) => combine(left, right, nulls, a).some
      case Record.Writer.One(_, field)           => one(field, nulls, a)
      case Record.Writer.Optional(self)          => optional(self, nulls, a)
      case Record.Writer.Transform(self, f)      => transform(self, f, nulls, a)

  def combine[A, B](
      left: Record.Writer.Via[Json, A],
      right: Record.Writer.Via[Json, B],
      nulls: Null,
      ab: (A, B)
  ): Chain[(String, Json)] = RecordJsonEncoder(left, nulls, ab._1).orEmpty ++
    RecordJsonEncoder(right, nulls, ab._2).orEmpty

  def one[A](field: Field.Writer.Via[Json, A], nulls: Null, a: A): Option[Chain[(String, Json)]] =
    FieldJsonEncoder(field, nulls, a).map(Chain.one)

  def optional[A](self: Record.Writer.Via[Json, A], nulls: Null, a: Option[A]): Option[Chain[(String, Json)]] =
    a.flatMap(RecordJsonEncoder(self, nulls, _))

  def transform[A, B](
      self: Record.Writer.Via[Json, A],
      f: B => A,
      nulls: Null,
      b: B
  ): Option[Chain[(String, Json)]] = RecordJsonEncoder(self, nulls, f(b))
