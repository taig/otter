package io.taig.otter.component

import io.taig.otter.Schema

trait SchemaComponent
    extends TupleComponent[
      Schema.Tuple.Of,
      Schema.Tuple.Read.Of,
      Schema.Tuple.Write.Of,
      Schema,
      Schema.Read,
      Schema.Write
    ]

object SchemaComponent extends SchemaComponent

object Playground:
  val schema: SchemaComponent = SchemaComponent

  val s: Schema[Int] = ???
  val sr: Schema.Read[Int] = ???
  val sw: Schema.Write[Int] = ???
  val p: Schema.Primitive[String] = ???
  val pr: Schema.Primitive.Read[String] = ???
  val pw: Schema.Primitive.Write[String] = ???
  val t: Schema.Tuple.Of[Schema.Primitive, String] = ???
  val tr: Schema.Tuple.Read.Of[Schema.Primitive.Read, String] = ???
  val tw: Schema.Tuple.Write.Of[Schema.Primitive.Write, String] = ???

  val _: Schema.Tuple.Of[Schema.Primitive, String] = p.toTuple
  val _: Schema.Tuple.Of[Schema.Tuple.Of[Schema.Primitive, *], String] = t.toTuple
  val _: Schema.Tuple.Read[String] = pr.toTuple
  val _ = pw.toTuple

  val _: Schema.Tuple[(String, Int)] = p :* s
  val _: Schema.Tuple[(Int, Int)] = s :* s
  val _: Schema.Tuple.Of[Schema.Primitive, (String, String)] = t :* p
  val _: Schema.Tuple[(String, String)] = p :* t
  val _: Schema.Tuple.Read[(String, String)] = p :* pr
  val _: Schema.Tuple.Read[(String, String)] = tr :* pr
  val _: Schema.Tuple.Read.Of[Schema.Read, (String, Int)] = pr *: sr
  val _: Schema.Tuple.Read.Of[Schema.Read, (Int, String)] = sr *: pr
  val _: Schema.Tuple.Write.Of[Schema.Write, (String, Int)] = pw *: sw
  val _: Schema.Tuple.Write.Of[Schema.Write, (Int, String)] = sw *: pw
  val _ = s *: pr
  val _ = pr *: s
  val _ = sr *: p
