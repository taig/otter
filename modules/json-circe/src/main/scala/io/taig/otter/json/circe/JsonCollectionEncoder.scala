package io.taig.otter.json.circe

import io.taig.otter as Base
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Collection
import io.taig.otter.Plain
import io.taig.otter.Fix
import scala.annotation.targetName

object JsonCollectionEncoder:
  def apply[A](schema: Plain.Collection.Writer[A], a: A): Option[Chain[Json]] = apply(schema.unfix, a)

  @targetName("applyBase")
  def apply[Of, A](schema: Base.Collection.Writer[Of, A], a: A): Option[Chain[Json]] = schema match
    case Base.Collection.Root(schema, writer)        => a.map(JsonEncoder(writer(schema), _)).some
    case Base.Collection.Writer.Root(schema, writer) => a.map(JsonEncoder(writer(schema), _)).some
    case Base.Collection.Validate(self, _, f)        => apply(self, f(a))
    case Base.Collection.Writer.Modify(self, f)      => apply(self, f(a))
    case Base.Collection.Optional(self)              => a.flatMap(apply(self, _))
    case Base.Collection.Writer.Optional(self)       => a.flatMap(apply(self, _))
