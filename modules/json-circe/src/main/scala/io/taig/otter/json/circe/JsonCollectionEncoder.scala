package io.taig.otter.json.circe

import io.taig.otter as Base
import io.circe.Json
import cats.syntax.all.*
import cats.Id as Identity

object JsonCollectionEncoder:
  def apply[A](
      data: Base.Collection[
        Base.Writer[Base.Schema[Base.Data[Identity, Base.Writer, ?, *], *], *],
        ?,
        ?,
        A
      ],
      a: A
  ): Vector[Json] = data match
    case Base.Collection.Root(schema) => a.map(JsonEncoder.apply(schema, _)).some
