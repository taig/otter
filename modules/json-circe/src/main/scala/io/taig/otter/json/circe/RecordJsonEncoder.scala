package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import cats.data.Chain

object RecordJsonEncoder:
  def apply[A](schema: Record.Writer[A], a: A): Option[Chain[(String, Json)]] =
    RecordJsonEncoder(schema, Record.Null.Default, a)

  def apply[A](schema: Record.Writer[A], nulls: Record.Null, a: A): Option[Chain[(String, Json)]] = schema match
    case Base.Record.Combine(left, right)        => combine(left, right, nulls, a).some
    case Base.Record.Writer.Combine(left, right) => combine(left, right, nulls, a).some
    case Base.Record.Empty                       => Chain.empty.some
    case Base.Record.One(field)                  => one(field, nulls, a)
    case Base.Record.Optional(self)              => optional(self, nulls, a)
    case Base.Record.Transform(self, _, f)       => transform(self, f, nulls, a)
    case Base.Record.Writer.One(field)           => one(field, nulls, a)
    case Base.Record.Writer.Optional(self)       => optional(self, nulls, a)
    case Base.Record.Writer.Transform(self, f)   => transform(self, f, nulls, a)
    case Base.Record.Nulls(self, nulls)          => RecordJsonEncoder(self, nulls, a)
    case Base.Record.Writer.Nulls(self, nulls)   => RecordJsonEncoder(self, nulls, a)

  def combine[A, B](
      left: Record.Writer[A],
      right: Record.Writer[B],
      nulls: Record.Null,
      ab: (A, B)
  ): Chain[(String, Json)] = RecordJsonEncoder(left, nulls, ab._1).orEmpty ++
    RecordJsonEncoder(right, nulls, ab._2).orEmpty

  def one[A](field: Field.Writer[A], nulls: Record.Null, a: A): Option[Chain[(String, Json)]] =
    FieldJsonEncoder(field, nulls, a).map(Chain.one)

  def optional[A](self: Record.Writer[A], nulls: Record.Null, a: Option[A]): Option[Chain[(String, Json)]] =
    a.flatMap(RecordJsonEncoder(self, nulls, _))

  def transform[A, B](self: Record.Writer[A], f: B => A, nulls: Record.Null, b: B): Option[Chain[(String, Json)]] =
    RecordJsonEncoder(self, nulls, f(b))
