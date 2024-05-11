// package io.taig.otter.json.circe

// import io.circe.Json
// import io.taig.otter.Encoder
// import io.taig.otter as Base
// import io.taig.otter.Schema
// import io.taig.otter.Tuple

// object JsonEncoder extends Encoder[Schema[?, *], Json]:
//   override def apply[A](schema: Schema[?, A], a: A): Json = schema match
//     case schema: Tuple[?, ?] => JsonTupleEncoder(schema, a).fold(Json.Null)(values => Json.fromValues(values.toVector))

// //   override def apply[B](schema: Schema[Write, Fix[Schema[Write, *, ?]], B], b: B): Json = ???

// //   def apply2[Of, A](schema: Schema[Write, Of, A], a: A): Json = schema match
// //     case schema: Collection[Write, Of, A] =>
// //       ??? // JsonCollectionEncoder(schema, a).fold(Json.Null)(values => Json.fromValues(values.toVector))

// // //   override def apply[A](schema: Plain.Schema.Writer[A], a: A): Json = apply(schema.unfix, a)

// // //   @targetName("applyBase")
// // //   def apply[Of, A](schema: Base.Schema.Writer[Of, A], a: A): Json = schema match
// // //     case schema: Base.Collection.Writer[Of, A] =>
// // //       JsonCollectionEncoder(schema, a).fold(Json.Null)(values => Json.fromValues(values.toVector))

// // // //   // schema match
// // // //   //   case schema: Collection.Writer[Fix[Base.Schema[*, ?]], A] =>
// // // //   //     JsonCollectionEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
// // // //   // case schema: Plain.Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
// // // //   //   case schema: Tuple.Writer[Fix[Base.Schema[*, ?]], A] =>
// // // //   //     JsonTupleEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
