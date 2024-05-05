// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.taig.otter.Tuple
// import io.circe.Json
// import cats.data.Chain
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import io.taig.otter.validation.Constraint
// import io.circe.syntax.*
// import io.taig.otter.Schema

// object JsonTupleDecoder:
//   def decode[A](schema: Tuple[Schema[?], A], values: Option[Chain[Json]]): Validated[Violations[Json], A] = values match
//     case Some(values) =>
//       val expected = schema.size.toLong
//       val actual = values.length
//       if expected > actual then Violations.rootNec(Violation(Constraint.MinItems(expected), actual.asJson)).invalid
//       else if expected < actual then Violations.rootNec(Violation(Constraint.MaxItems(expected), actual.asJson)).invalid
//       else decodeWithRemainders(schema, values).map { case (_, a) => a }
//     case None =>
//       schema match
//         case Tuple.Optional(_) => none.valid[Violations[Json]]
//         case _                 => Violations.rootNec(Violation(Constraint.Required, Json.Null)).invalid

//   def decodeWithRemainders[A](
//       schema: Tuple[Schema[?], A],
//       values: Chain[Json]
//   ): Validated[Violations[Json], (Chain[Json], A)] = schema match
//     case Tuple.Empty => (Chain.empty, ()).valid
//     case Tuple.Validate(schema, constraint, validation, _) =>
//       decodeWithRemainders(schema, values).andThen:
//         _.traverse: a =>
//           validation(a)
//             .leftMap(_.map(_.map(JsonEncoder.encode(constraint, _))))
//             .leftMap(Violations.root)
//     case Tuple.One(schema) =>
//       values.uncons match
//         case Some((head, tail)) => JsonDecoder.decode(schema, head).tupleLeft(tail)
//         case None               => Violations.rootNec(Violation(Constraint.MinItems(1), actual = 0.asJson)).invalid
//     case Tuple.Optional(schema) => decodeWithRemainders(schema, values).map(_.map(_.some))
//     case Tuple.Product(left, right) =>
//       decodeWithRemainders(left, values).andThen { case (remainders, a) =>
//         decodeWithRemainders(right, remainders).map(_.tupleLeft(a))
//       }
