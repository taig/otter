package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.Collection
import io.taig.otter.Encoder

object JsonEncoder extends Encoder[Schema.Write.Any[?, *], Json]:
  override def apply[A](schema: Schema.Write.Any[?, A], a: A): Json = ???
  //   JsonCollectionEncoder.encode(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)

  // override def encode[A](schema: Primitive[A], a: A): Json = JsonPrimitiveEncoder.encode(schema, a)

  // override def encode[A](schema: Tuple[Schema[?], A], a: A): Json =
  //   JsonTupleEncoder.encode(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)

  // override def encode[A](schema: Union[Schema[?], A], a: A): Json = JsonUnionEncoder.encode(schema, a)
