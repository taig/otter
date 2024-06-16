// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.circe.Json
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import io.taig.otter.Plain.*
// import io.taig.otter.Decoder
// import io.taig.otter as Base
// import io.taig.otter.validation.Violation
// import io.circe.syntax.*

// object JsonDecoder extends Decoder[Schema.Reader, Json]:
//   override def apply[A](schema: Schema.Reader[A], json: Json): Validated[Violations[Json, Json], A] = schema match
//     case schema: Collection.Reader[A] =>
//       if json.isNull then JsonCollectionDecoder(schema, none)
//       else
//         json.asArray match
//           case Some(array) => JsonCollectionDecoder(schema, array.some)
//           case None => Violations.rootNec(Violation.tpe(name = "array", actual = typeOf(json)).map(_.asJson)).invalid
//     case schema: Primitive.Reader[A] => JsonPrimitiveDecoder(schema, json)
//     case schema: Tuple.Reader[A] =>
//       if json.isNull then JsonTupleDecoder(schema, none)
//       else
//         json.asArray match
//           case Some(array) => JsonTupleDecoder(schema, array.some)
//           case None => Violations.rootNec(Violation.tpe(name = "array", actual = typeOf(json)).map(_.asJson)).invalid
//     case schema: Union.Reader[A] => JsonUnionDecoder(schema, json)
