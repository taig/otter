package io.taig.otter.json.circe

import cats.Id
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Tuple
import io.taig.otter.Plain
import io.taig.otter.Schema

object JsonTupleEncoder:
  def apply[A](schema: Plain.Tuple.Writer[A], a: A): Option[Chain[Json]] = schema match
    case Tuple.Empty                => Chain.empty.some
    case Tuple.One(schema)          => Chain.one(JsonEncoder(schema, a)).some
    case Tuple.Product(left, right) => apply(left, a._1) |+| apply(right, a._2)
    // case Tuple.Optional(self)       => b.flatMap(apply(self, _))

  def apply2[A, B, C, D](schema: Tuple.Product[Id, A, B, C, D], a: (B, D)) = ???
  // val x: Plain.Tuple[?] = schema.left
  // apply(x, ???)
