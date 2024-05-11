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

  def apply2[A, B](schema: Base.Collection.Writer[Fix[Schema.Writer[*, ?]], B], a: A): Option[Chain[Json]] =
    schema match
      // case Base.Collection.Writer.Root(schema)                                   => ???
      case schema: Base.Collection.Writer.Root[[a] =>> Schema.Writer[a, Any], B] => ???

  val a: Plain.Primitive.Writer[String] = ???

  // val x = Collection.Writer.Root[Fix, String](a)

  // val a: Collection.Writer[Fix[Schema.Writer[*, String]], Chain[String]] = ???

  // a match
  //   // case Collection.Writer.Root2(schema) =>
  //   //   schema.unfix
  //   case schema: Collection.Writer.Root[Fix[Schema.Writer[*, ?]], String] => ???
