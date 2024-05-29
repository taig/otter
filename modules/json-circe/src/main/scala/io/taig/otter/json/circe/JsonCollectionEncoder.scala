package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*
import cats.Id as Identity

object JsonCollectionEncoder:
  def apply[A](
      data: Base.Collection[Identity, Base.Writer, Parent.Writer, A],
      a: A
  ): Vector[Json] = data match
    case Base.Collection.Root(schema) =>
      // val x: AsSchema[Base.Writer[Base.Schema[Base.Data[Identity, Base.Writer, Parent.Writer, *], *], A]] = schema
      val x: AsSchema[Base.Writer[Base.Schema[Base.Data[Identity, Base.Writer, ?, *], *], A]] = schema
      // a.map(JsonEncoder.apply0(x, _))
      ???
    // a.map(JsonEncoder.apply0(schema, _)).some
