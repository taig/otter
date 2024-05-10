package io.taig.otter.json.circe

import io.taig.otter.Collection
import io.taig.otter as Base
import cats.data.Chain
import io.taig.otter.Fix
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Plain
import io.taig.otter.Schema

object JsonCollectionEncoder:
  def apply2[A](schema: Plain.Collection.Writer[A], a: A): Option[Chain[Json]] = apply(schema.unfix, a)

  def apply[A](schema: Base.Collection.Writer[Fix[Base.Schema.Writer[*, ?]], A], a: A): Option[Chain[Json]] =
    schema match
      case Collection.Writer.Root(schema)    => a.map(JsonEncoder(schema, _)).some
      case Collection.Writer.Modify(self, f) => apply(self, f(a))
      case Collection.Writer.Optional(self)  => a.flatMap(apply(self, _))
      case Collection(_, asWrite)            => apply(asWrite, a)
