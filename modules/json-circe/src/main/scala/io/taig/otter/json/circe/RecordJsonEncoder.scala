package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import cats.data.Chain

object RecordJsonEncoder:
  def apply[A](schema: Record.Writer[A], a: A): Option[Chain[(String, Json)]] = schema match
    case Base.Record.Combine(left, right)      => combine(left, right, a).some
    case Base.Record.Empty                     => Chain.empty.some
    case Base.Record.One(field)                => one(field, a)
    case Base.Record.Optional(self)            => optional(self, a)
    case Base.Record.Transform(self, _, f)     => transform(self, f, a)
    case Base.Record.Writer.One(field)         => one(field, a)
    case Base.Record.Writer.Optional(self)     => optional(self, a)
    case Base.Record.Writer.Transform(self, f) => transform(self, f, a)

  def combine[A, B](left: Record.Writer[A], right: Record.Writer[B], ab: (A, B)): Chain[(String, Json)] =
    RecordJsonEncoder(left, ab._1).orEmpty ++ RecordJsonEncoder(right, ab._2).orEmpty

  def one[A](field: Field.Writer[A], a: A): Option[Chain[(String, Json)]] =
    FieldJsonEncoder(field, a).map(Chain.one)

  def optional[A](self: Record.Writer[A], a: Option[A]): Option[Chain[(String, Json)]] =
    a.flatMap(RecordJsonEncoder(self, _))

  def transform[A, B](self: Record.Writer[A], f: B => A, b: B): Option[Chain[(String, Json)]] =
    RecordJsonEncoder(self, f(b))
