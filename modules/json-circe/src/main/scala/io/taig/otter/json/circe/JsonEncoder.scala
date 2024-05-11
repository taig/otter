package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Collection
import io.taig.otter.Encoder
import io.taig.otter.Primitive
import io.taig.otter.Tuple
import io.taig.otter.Plain
import io.taig.otter as Base
import io.taig.otter.Fix

object JsonEncoder extends Encoder[Plain.Schema.Writer, Json]:
  override def apply[A](schema: Plain.Schema.Writer[A], a: A): Json = ???
//   // schema match
//   //   case schema: Collection.Writer[Fix[Base.Schema[*, ?]], A] =>
//   //     JsonCollectionEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
//   // case schema: Plain.Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
//   //   case schema: Tuple.Writer[Fix[Base.Schema[*, ?]], A] =>
//   //     JsonTupleEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
