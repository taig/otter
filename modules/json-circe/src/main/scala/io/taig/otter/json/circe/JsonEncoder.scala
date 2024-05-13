package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Encoder
import io.taig.otter.Plain
import io.taig.otter.Plain.*
import io.taig.otter.Fix
import scala.annotation.targetName

object JsonEncoder: // extends Encoder[Plain.Schema.Writer, Json]:
  def apply[A](schema: Schema.Writer[A], a: A): Json = ???

  // @targetName("applyBase")
  // def apply2[A, B](schema: Base.Schema.Writer[Fix[Base.Schema.Writer[*, B]], A], a: A): Json = schema match
  //   case schema: Plain.Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
  //   case schema: Plain.Tuple.Writer[A] =>
  //     JsonTupleEncoder(schema, a).fold(Json.Null)(values => Json.fromValues(values.toVector))
