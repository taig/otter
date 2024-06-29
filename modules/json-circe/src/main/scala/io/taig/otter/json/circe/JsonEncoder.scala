package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Encoder
import io.taig.otter.Plain.*

object JsonEncoder extends Encoder[Schema.Writer, Json]:
  override def apply[A](schema: Schema.Writer[A], a: A): Json = schema match
    case schema: Primitive.Writer[A]  => JsonPrimitiveEncoder(schema, a)
    case schema: Collection.Writer[A] => ???
  // schema match
  //   case schema: Collection.Writer[A] => JsonCollectionEncoder(schema, a).fold(Json.Null)(Json.fromValues)
  //   case schema: Primitive.Writer[A]  => JsonPrimitiveEncoder(schema, a)
  // //   case schema: Tuple.Writer[A]      => JsonTupleEncoder(schema, a).fold(Json.Null)(Json.fromValues)
  // // case schema: Union.Writer[A]      => JsonUnionEncoder(schema, a)
