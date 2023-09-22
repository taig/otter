package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Schema.Collection

object StringEncoder:
  val value: Encoder[Schema.Value, Option[String]] = new Encoder:
    override def encode[B](schema: Schema.Value[B], b: B): Option[String] = schema match
      case schema: Schema.Enumeration[B] => ???
      case schema: Schema.Primitive[B]   => ???

  val collection: Encoder[Schema.Collection[Schema.Value, *], Option[Chain[String]]] = new Encoder:
    // Unsafe because mapFilter throws nulls away
    override def encode[B](schema: Schema.Collection[Schema.Value, B], b: B): Option[Chain[String]] = schema match
      case Collection.Root(schema, _, _)   => b.mapFilter(value.encode(schema, _)).some
      case Collection.Optional(self)       => b.flatMap(encode(self, _))
      case Collection.Validate(self, _, g) => encode(self, g(b))
