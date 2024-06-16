// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.taig.otter as Base
// import io.taig.otter.Plain.*
// import io.circe.Json
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import io.circe.syntax.*
// import io.taig.otter.SchemaValidation

// object JsonTupleDecoder:
//   def apply[A](schema: Tuple.Reader[A], values: Option[Vector[Json]]): Validated[Violations[Json, Json], A] =
//     applyWithRemainders(schema, values).map(_._2)

//   // TODO add index to errors
//   def applyWithRemainders[A](
//       schema: Tuple.Reader[A],
//       values: Option[Vector[Json]]
//   ): Validated[Violations[Json, Json], (Option[Vector[Json]], A)] = schema match
//     case Base.Tuple.Empty                           => empty(values)
//     case Base.Tuple.Modify(self, validation, _)     => modify(self, validation, values)
//     case Base.Tuple.One(schema)                     => one(schema, values)
//     case Base.Tuple.Optional(self)                  => optional(self, values)
//     case Base.Tuple.Product(left, right)            => product(left, right, values)
//     case Base.Tuple.Reader.Empty                    => empty(values)
//     case Base.Tuple.Reader.Modify(self, validation) => modify(self, validation, values)
//     case Base.Tuple.Reader.One(schema)              => one(schema, values)
//     case Base.Tuple.Reader.Optional(self)           => optional(self, values)
//     case Base.Tuple.Reader.Product(left, right)     => product(left, right, values)

//   def empty(values: Option[Vector[Json]]): Validated[Violations[Json, Json], (Option[Vector[Json]], Unit)] =
//     (values, ()).valid

//   def modify[A, V1, V2, B](
//       self: Tuple.Reader[A],
//       validation: SchemaValidation[A, V1, V2, B],
//       values: Option[Vector[Json]]
//   ): Validated[Violations[Json, Json], (Option[Vector[Json]], B)] = applyWithRemainders(self, values).andThen:
//     case (remainders, a) =>
//       validation(a)
//         .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
//         .leftMap(Violations.root)
//         .tupleLeft(remainders)

//   def one[A](
//       schema: Schema.Reader[A],
//       values: Option[Vector[Json]]
//   ): Validated[Violations[Json, Json], (Option[Vector[Json]], A)] = values
//     .toValid(Violations.rootNec(Violation.tpe(name = "array", actual = "null").map(_.asJson)))
//     .andThen: values =>
//       values.headOption match
//         case Some(head) => JsonDecoder(schema, head).tupleLeft(values.tail.some)
//         case None       => Violations.rootNec(Violation.minItems(reference = 1, actual = 0).map(_.asJson)).invalid

//   def optional[A](
//       self: Tuple.Reader[A],
//       values: Option[Vector[Json]]
//   ): Validated[Violations[Json, Json], (Option[Vector[Json]], Option[A])] =
//     values.fold((none, none).valid)(_ => applyWithRemainders(self, values).map(_.map(_.some)))

//   def product[A, B](
//       left: Tuple.Reader[A],
//       right: Schema.Reader[B],
//       values: Option[Vector[Json]]
//   ): Validated[Violations[Json, Json], (Option[Vector[Json]], (A, B))] =
//     applyWithRemainders(left, values).andThen { case (remainders, a) =>
//       remainders match
//         case Some(values) =>
//           values.headOption match
//             case Some(head) => JsonDecoder(right, head).map(b => (values.tail.some, (a, b)))
//             case None       => Violations.rootNec(???).invalid
//         case None => ???
//     }
