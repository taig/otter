package io.taig.otter.codec

import cats.Order
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Primitive
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

final class PrimitiveDecoder[S[_], T](decoder: Decoder[Primitive[S, *], T]) extends Decoder[Primitive[S, *], T]:
  given Order[JBigInteger] = Order.fromComparable
  given Order[JBigDecimal] = Order.fromComparable

  override def decode[A](schema: Primitive[S, A], value: T): Validated[Violations, A] = schema match
    case schema @ Primitive.Number(Primitive.Value.Number.BigDecimal(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.BigInteger(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Double(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Float(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Int(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Long(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema @ Primitive.String(Primitive.Value.String.Text(validation), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          validation
            .validate(value)
            .toValidated
            .leftMap(Violation.fromConstraints(_, actual = value))
            .leftMap(Violations.rootNec)
            .as(value)
    case schema => decoder.decode(schema, value)
