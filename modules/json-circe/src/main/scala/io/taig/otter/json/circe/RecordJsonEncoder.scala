package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.JsonObject

object RecordJsonEncoder:
  def apply[A](schema: Record.Writer[A], a: A): Option[JsonObject] = schema match
    case Base.Record.Empty                     => JsonObject.empty.some
    case Base.Record.One(field)                => JsonObject(FieldJsonEncoder(field, a)).some
    case Base.Record.Optional(self)            => optional(self, a)
    case Base.Record.Transform(self, _, f)     => transform(self, f, a)
    case Base.Record.Writer.One(field)         => JsonObject(FieldJsonEncoder(field, a)).some
    case Base.Record.Writer.Optional(self)     => optional(self, a)
    case Base.Record.Writer.Transform(self, f) => transform(self, f, a)

  def optional[A](self: Record.Writer[A], a: Option[A]): Option[JsonObject] =
    a.flatMap(RecordJsonEncoder(self, _))

  def transform[A, B](self: Record.Writer[A], f: B => A, b: B): Option[JsonObject] =
    RecordJsonEncoder(self, f(b))
