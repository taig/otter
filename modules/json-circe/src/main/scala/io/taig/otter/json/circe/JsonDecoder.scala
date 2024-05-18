// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.circe.Json
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import io.circe.syntax.*
// import io.taig.otter.Plain.*
// import io.taig.otter.Decoder

// object JsonDecoder extends Decoder[Schema.Reader, Json]:
//   override def apply[A](schema: Schema.Reader[A], json: Json): Validated[Violations[Json, Json], A] =
//     schema match
//       case schema: Primitive.Reader[A] => primitive(schema, json)
//       case schema: Tuple.Reader[A]     => tuple(schema, json)

//   def primitive[A](schema: Primitive.Reader[A], json: Json): Validated[Violations[Json, Json], A] =
//     JsonPrimitiveDecoder(schema, json)

//   def tuple[A](schema: Tuple.Reader[A], json: Json): Validated[Violations[Json, Json], A] =
//     if json.isNull then JsonTupleDecoder(schema, none)
//     else
//       json.asArray match
//         case Some(values) => JsonTupleDecoder(schema, values.some)
//         case None         => Violations.rootNec(Violation.tpe("array", typeOf(json)).map(_.asJson)).invalid
