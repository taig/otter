package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

object JsonCollectionEncoder:
  def apply[A](data: Base.Collection[Parent.Writer.Any, A], a: A): Vector[Json] =
    data match
      case data: Base.Collection.Root[Parent.Writer, ?] => apply1(data, a)
      // case Base.Collection.Root(schema)  => a.map(JsonEncoder.apply0(schema, _))

  def apply1[A](data: Base.Collection.Root[Parent.Writer, A], a: Vector[A]): Vector[Json] = ???
