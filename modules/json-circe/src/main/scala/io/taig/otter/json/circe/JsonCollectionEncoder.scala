package io.taig.otter.json.circe

import io.taig.otter as Base
import cats.data.Chain
import io.taig.otter.Fix
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.Collection
import io.taig.otter.Plain

object JsonCollectionEncoder:
  def apply[A](schema: Plain.Collection.Writer[A], a: A): Option[Chain[Json]] = apply2(schema.unfix, a)

  def apply2[Of, A](schema: Base.Collection.Writer[Of, A], a: A): Option[Chain[Json]] =
    schema match
      case Base.Collection.Writer.Root(schema, writer) =>
        // apply3(schema, a)
        a.map(fake(writer(schema), _)).some

  def apply3[A, Of](schema: Schema.Writer[Of, A], a: Chain[A]): Option[Chain[Json]] = ???

  // def apply3[A](schema: Base.Collection.Writer.Root[Fix[Schema.Writer[*, ?]], A], a: Chain[A]): Option[Chain[Json]] =
  //   a.map(fake(schema.writer(schema.schema), _)).some

  def fake[A](schema: Schema.Writer[?, A], a: A): Json = ???
