// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.taig.otter.Tuple
// import io.circe.Json
// import cats.data.Chain
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import io.circe.syntax.*
// import io.taig.otter.Schema
// import io.taig.otter.SchemaValidation
// import io.taig.otter.Fix

// object JsonTupleDecoder:
//   def apply[A, B](
//       schema: Tuple.Reader[JsonSchema.Reader[A], B],
//       values: Option[Chain[Json]]
//   ): Validated[Violations[Json, Json], B] = values match
//     case Some(values) =>
//       val expected = schema.schemas.size
//       val actual = values.length
//       if expected > actual then
//         Violations.rootNec(Violation.minItems(reference = expected, actual).map(_.asJson)).invalid
//       else if expected < actual then
//         Violations.rootNec(Violation.maxItems(reference = expected, actual).map(_.asJson)).invalid
//       else applyWithRemainders(schema, values)._2
//     case None =>
//       schema match
//         case Tuple.Optional(_) => none.valid[Violations[Json, Json]]
//         case _                 => Violations.rootNec(Violation.tpe("array", "null").map(_.asJson)).invalid

//   def applyWithRemainders[A, B](
//       schema: Tuple.Reader[JsonSchema.Reader[A], B],
//       values: Chain[Json]
//   ): (Chain[Json], Validated[Violations[Json, Json], B]) = schema match
//     case Tuple.Empty                               => (Chain.empty, ().valid)
//     case Tuple.Reader.Empty                        => (Chain.empty, ().valid)
//     case Tuple.Reader.One(schema)                  => oneWithRemainders(schema.unfix, values)
//     case Tuple.One(schema)                         => oneWithRemainders(schema.unfix, values)
//     case Tuple.Validate(schema, validation, _)     => validateWithRemainders(schema, validation, values)
//     case Tuple.Reader.Validate(schema, validation) => validateWithRemainders(schema, validation, values)
//     case Tuple.Optional(schema)                    => applyWithRemainders(schema, values).map(_.map(_.some))
//     case Tuple.Reader.Optional(schema)             => applyWithRemainders(schema, values).map(_.map(_.some))
//     case Tuple.Product(left, right)                => productWithRemainders(left, right, values)
//     case Tuple.Reader.Product(left, right)         => productWithRemainders(left, right, values)

//   def oneWithRemainders[A](
//       schema: Schema.Reader[JsonSchema.Reader[A], A],
//       values: Chain[Json]
//   ): (Chain[Json], Validated[Violations[Json, Json], A]) = values.uncons match
//     case Some((head, tail)) => (tail, JsonDecoder(Fix(schema), head))
//     case None => (Chain.empty, Violations.rootNec(Violation.minItems(reference = 1, actual = 0).map(_.asJson)).invalid)

//   def validateWithRemainders[A, B, C, D, E](
//       schema: Tuple.Reader[JsonSchema.Reader[A], B],
//       validation: SchemaValidation[B, C, D, E],
//       values: Chain[Json]
//   ): (Chain[Json], Validated[Violations[Json, Json], E]) = applyWithRemainders(schema, values).map:
//     _.andThen:
//       validation(_)
//         .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
//         .leftMap(Violations.root)

//   def productWithRemainders[A, B, C](
//       left: Tuple.Reader[JsonSchema.Reader[A], B],
//       right: Tuple.Reader[JsonSchema.Reader[A], C],
//       values: Chain[Json]
//   ): (Chain[Json], Validated[Violations[Json, Json], (B, C)]) =
//     val (bs, b) = applyWithRemainders(left, values)
//     val (cs, c) = applyWithRemainders(right, bs)
//     (cs, b.product(c))
