package io.taig.otter.component

import io.taig.otter.Schema

trait SchemaComponent // extends TupleComponent[Schema.Tuple.Of, Schema]

object SchemaComponent extends SchemaComponent

object Playground:
  val schema = SchemaComponent

  val t: Schema.Tuple.Of[Schema.Tuple, String] = ???
  val tr: Schema.Tuple.Read.Of[Schema.Tuple.Read, String] = ???
  val tw: Schema.Tuple.Write.Of[Schema.Tuple.Write, String] = ???

  // val _: Schema.Tuple.Of[Schema.Tuple, (String, String)] = t :* t
  // val _: Schema.Tuple.Read.Of[Schema.Tuple.Read, (String, String)] = tr :* tr
  // val _: Schema.Tuple.Write.Of[Schema.Tuple.Write, (String, String)] = tw :* tw

  val _ = tr :* t
  val _ = t :* tr
