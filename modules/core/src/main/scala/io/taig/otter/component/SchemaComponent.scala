package io.taig.otter.component

import io.taig.otter.Schema
import cats.data.NonEmptyList

trait SchemaComponent
    extends CollectionComponent[
      Schema.Collection.Of,
      Schema.Collection.Read.Of,
      Schema.Collection.Write.Of,
      Schema,
      Schema.Read,
      Schema.Write
    ],
      TupleComponent[
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

  val _: Schema.Collection[List[Int]] = schema.collection.list(s)
  val _: Schema.Collection.Of[Schema.Primitive, List[String]] = schema.collection.list(p)
  val _: Schema.Collection.Read[List[Int]] = schema.collection.list(sr)
  val _: Schema.Collection.Read.Of[Schema.Primitive.Read, List[String]] = schema.collection.list(pr)
  val _: Schema.Collection.Write[List[Int]] = schema.collection.list(sw)
  val _: Schema.Collection.Write.Of[Schema.Primitive.Write, List[String]] = schema.collection.list(pw)
  val _: Schema.Collection[NonEmptyList[Int]] = schema.collection.nonEmptyList(s)

  val _: Schema.Tuple.Of[Schema.Primitive, String] = p.toTuple
  val _: Schema.Tuple.Of[Schema.Tuple.Of[Schema.Primitive, *], String] = t.toTuple
  val _: Schema.Tuple.Read[String] = pr.toTuple
  val _: Schema.Tuple.Write[String] = pw.toTuple

  val _: Schema.Tuple[(Int, Int)] = s :* s
  val _: Schema.Tuple[(String, Int)] = p :* s
  val _: Schema.Tuple.Of[Schema.Primitive, (String, String)] = t :* p
  val _: Schema.Tuple[(String, String)] = p :* t
  val _: Schema.Tuple.Read[(String, String)] = p :* pr
  val _: Schema.Tuple.Read[(String, String)] = tr :* pr
  val _: Schema.Tuple.Write[(Int, String)] = s :* pw
  val _: Schema.Tuple.Read.Of[Schema.Read, (String, Int)] = pr *: sr
  val _: Schema.Tuple.Read.Of[Schema.Read, (Int, String)] = sr *: pr
  val _: Schema.Tuple.Write.Of[Schema.Write, (String, Int)] = pw *: sw
  val _: Schema.Tuple.Write.Of[Schema.Write, (Int, String)] = sw *: pw
  val _: Schema.Tuple.Read[(Int, String)] = s *: pr
  val _: Schema.Tuple.Read.Of[Schema.Read, (String, Int)] = pr :* s
  val _: Schema.Tuple.Read.Of[Schema.Read, (String, Int)] = pr *: s
  val _: Schema.Tuple.Read[(Int, String)] = sr *: p
  val _: Schema.Tuple.Write.Of[Schema.Write, (String, Int)] = pw *: s
  val _: Schema.Tuple.Write[(Int, String)] = sw *: p

  // val _: Schema.Tuple.Of[Schema.Primitive, (String, String, String)] = p :* p :* p
  val _: Schema.Tuple.Of[Schema.Primitive, (String, String, String)] = p *: p *: p
  val _: Schema.Tuple.Read.Of[Schema.Primitive.Read, (String, String, String)] = pr :* pr :* pr
  val _: Schema.Tuple.Read.Of[Schema.Primitive.Read, (String, String, String)] = pr *: pr *: pr
  val _: Schema.Tuple.Write.Of[Schema.Primitive.Write, (String, String, String)] = pw :* pw :* pw
  val _: Schema.Tuple.Write.Of[Schema.Primitive.Write, (String, String, String)] = pw *: pw *: pw
  // val _: Schema.Tuple.Read.Of[Schema.Primitive.Read, (String, String, String)] = p :* p :* pr
  val _: Schema.Tuple.Read.Of[Schema.Primitive.Read, (String, String, String)] = pr *: p *: p
  val _: Schema.Tuple.Write.Of[Schema.Primitive.Write, (String, String, String)] = pw *: p *: p
