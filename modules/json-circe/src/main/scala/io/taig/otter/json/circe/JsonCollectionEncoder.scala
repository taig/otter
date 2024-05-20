package io.taig.otter.json.circe

import io.taig.otter as Base
import io.circe.Json
import cats.syntax.all.*
import cats.Id as Identity

object JsonCollectionEncoder:
  def apply[A](data: Base.Collection[Identity, A], a: A): Option[Vector[Json]] = data match
    case Base.Collection.Root(schema) => a.map(JsonEncoder(schema, _)).some
