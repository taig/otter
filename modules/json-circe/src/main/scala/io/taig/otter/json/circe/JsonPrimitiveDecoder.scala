// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.taig.otter.*
// import io.circe.Json
// import io.circe.Decoder as CirceDecoder
// import cats.data.Validated
// import io.taig.otter.validation.Violations
// import java.math.BigDecimal as JBigDecimal
// import java.math.BigInteger as JBigInteger
// import io.taig.otter.validation.Constraint
// import io.taig.otter.validation.Violation
// import io.circe.syntax.*

// object JsonPrimitiveDecoder:
//   def decode[A](schema: Primitive[A], json: Json): Validated[Violations[Json], A] = schema match
//     case Primitive.Required.Root(tpe) =>
//       decode(tpe, json).toValidated
//         .leftMap(_ => Violations.rootNec(Violation(Constraint.Type(typeOf(tpe)), typeOf(json).asJson)))
//     case Primitive.Required.Validate(schema, constraint, validation, _) =>
//       decode(schema, json).andThen: a =>
//         validation(a)
//           .leftMap(_.map(_.map(JsonEncoder.encode(constraint, _))))
//           .leftMap(Violations.root)
//     case Primitive.Optional.Root(schema) =>
//       if json.isNull then none.valid[Violations[Json]] else decode(schema, json).map(_.some)
//     case Primitive.Optional.Validate(schema, constraint, validation, _) =>
//       decode(schema, json).andThen: a =>
//         validation(a)
//           .leftMap(_.map(_.map(JsonEncoder.encode(constraint, _))))
//           .leftMap(Violations.root)

//   def decode[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
//     case Type.BigDecimal => json.as[JBigDecimal]
//     case Type.BigInteger => json.as[JBigInteger]
//     case Type.Boolean    => json.as[Boolean]
//     case Type.Double     => json.as[Double]
//     case Type.Float      => json.as[Float]
//     case Type.Int        => json.as[Int]
//     case Type.Long       => json.as[Long]
//     case Type.String     => json.as[String]
