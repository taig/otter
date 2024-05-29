package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*
import cats.Id as Identity

object JsonCollectionEncoder:
  def apply[A](data: Base.Collection[Identity, Base.Writer, Parent.Writer, A], a: A): Vector[Json] = data match
    case Base.Collection.Root(schema) => a.map(JsonEncoder(schema.self, _))
