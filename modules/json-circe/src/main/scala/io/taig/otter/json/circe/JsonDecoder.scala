// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.taig.otter.*
// import io.circe.Json
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import cats.data.Chain
// import io.taig.otter.validation.Violation
// import io.taig.otter.validation.Constraint
// import io.circe.syntax.*

// object JsonDecoder extends Decoder[Json]:
//   override def decode[A](schema: Collection[Schema[?], A], json: Json): Validated[Violations[Json], A] =
//     if json.isNull then JsonCollectionDecoder.decode(schema, none)
//     else
//       json.asArray match
//         case Some(values) => JsonCollectionDecoder.decode(schema, Chain.fromSeq(values).some)
//         case None         => Violations.rootNec(Violation(Constraint.Type("array"), typeOf(json).asJson)).invalid

//   override def decode[A](schema: Primitive[A], json: Json): Validated[Violations[Json], A] =
//     JsonPrimitiveDecoder.decode(schema, json)

//   override def decode[A](schema: Tuple[Schema[?], A], json: Json): Validated[Violations[Json], A] =
//     if json.isNull then JsonTupleDecoder.decode(schema, none)
//     else
//       json.asArray match
//         case Some(values) => JsonTupleDecoder.decode(schema, Chain.fromSeq(values).some)
//         case None         => Violations.rootNec(Violation(Constraint.Type("array"), typeOf(json).asJson)).invalid

//   override def decode[A](schema: Union[Schema[?], A], json: Json): Validated[Violations[Json], A] =
//     JsonUnionDecoder.decode(schema, json)
