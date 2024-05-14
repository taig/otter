package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*
import cats.Id
import io.taig.otter.Tuple.Empty
import io.taig.otter.Tuple.One
import io.taig.otter.Codec
import io.taig.otter.Fix

type Schema[A] = Base.Schema[Base.Codec[Id, ?, *], A]

object JsonEncoder: // extends Encoder[Plain.Schema.Writer, Json]:
  def apply1[A](schema: Schema[A], a: A): Json = schema match
    case Base.Schema.Root(schema)   => ??? // apply3(schema, a)
    case Base.Schema.Optional(self) => a.map(apply1(self, _)).getOrElse(Json.Null)

  def apply2[A](schema: Base.Codec[Id, ?, A], a: A): Json = schema match
    case schema: Base.Collection[?, ?, A] => ???
    case schema: Base.Primitive[A]        => ???
    case schema: Base.Tuple[Id, ?, A]     => apply3(schema, a)

  def apply3[A](tuple: Base.Tuple[Id, ?, A], a: A): Json = tuple match
    case Base.Tuple.Empty       => Json.arr()
    case Base.Tuple.One(schema) => apply1(schema, a)
