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
  def apply[A](schema: Plain.Collection.Writer[A], a: A): Option[Chain[Json]] = schema match
    case Collection.Writer.Root(schema) =>
      ??? // a.map(JsonEncoder(schema.unfix, _)).some
    case Fix(Collection.Writer.Modify(self, f)) => apply(Fix(self), f(a))
    case Collection.Writer.Optional(self)       => ??? // a.flatMap(apply(Fix(self), _))
    case Collection(_, asWrite)                 => ??? // apply(Fix(asWrite), a)

  def apply2[A](
      schema: Collection.Writer.Root[[x] =>> Fix[[_] =>> Schema.Writer[Fix[Schema.Writer[*, ?]], x]], A],
      a: Chain[A]
  ): Option[Chain[Json]] = ???
