package io.taig.otter.codec

import cats.Order
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Comparison
import io.taig.otter.Constraint
import io.taig.otter.Primitive
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

final class PrimitiveDecoder[S[_], T](decoder: Decoder[Primitive[S, *], T]) extends Decoder[Primitive[S, *], T]:
  given Order[JBigInteger] = Order.fromComparable
  given Order[JBigDecimal] = Order.fromComparable

  override def decode[A](schema: Primitive[S, A], value: T): Validated[Violations, A] = schema match
    case schema @ Primitive.Number(Primitive.Value.Number.BigDecimal(minimum, maximum, multiple), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          (minimum.traverse { minimum =>
            Validated.cond(
              test = minimum.gt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((minimum.widen)), value, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = maximum.lt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((maximum.widen)), value, hint = none))
            )
          } *> multiple.traverse { multiple =>
            Validated.cond(
              test = value.remainder(multiple) === JBigDecimal.ZERO,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Multiple(multiple), value, hint = none))
            )
          }).as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.BigInteger(minimum, maximum, multiple), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          (minimum.traverse { minimum =>
            Validated.cond(
              test = minimum.gt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((minimum.widen)), value, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = maximum.lt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((maximum.widen)), value, hint = none))
            )
          } *> multiple.traverse { multiple =>
            Validated.cond(
              test = value.remainder(multiple) === JBigInteger.ZERO,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Multiple(multiple), value, hint = none))
            )
          }).as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Double(minimum, maximum, multiple), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          (minimum.traverse { minimum =>
            Validated.cond(
              test = minimum.gt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((minimum.widen)), value, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = maximum.lt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((maximum.widen)), value, hint = none))
            )
          } *> multiple.traverse { multiple =>
            Validated.cond(
              test = value % multiple === 0,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Multiple(multiple), value, hint = none))
            )
          }).as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Float(minimum, maximum, multiple), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          (minimum.traverse { minimum =>
            Validated.cond(
              test = minimum.gt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((minimum.widen)), value, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = maximum.lt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((maximum.widen)), value, hint = none))
            )
          } *> multiple.traverse { multiple =>
            Validated.cond(
              test = value % multiple === 0,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Multiple(multiple), value, hint = none))
            )
          }).as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Int(minimum, maximum, multiple), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          (minimum.traverse { minimum =>
            Validated.cond(
              test = minimum.gt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((minimum.widen)), value, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = maximum.lt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((maximum.widen)), value, hint = none))
            )
          } *> multiple.traverse { multiple =>
            Validated.cond(
              test = value % multiple === 0,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Multiple(multiple), value, hint = none))
            )
          }).as(value)
    case schema @ Primitive.Number(Primitive.Value.Number.Long(minimum, maximum, multiple), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          (minimum.traverse { minimum =>
            Validated.cond(
              test = minimum.gt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((minimum.widen)), value, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = maximum.lt(value),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Minimum((maximum.widen)), value, hint = none))
            )
          } *> multiple.traverse { multiple =>
            Validated.cond(
              test = value % multiple === 0,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.Number.Multiple(multiple), value, hint = none))
            )
          }).as(value)
    case schema @ Primitive.String(Primitive.Value.String.Text(minimum, maximum, matches), _) =>
      decoder
        .decode(schema, value)
        .andThen: value =>
          val length = value.length

          (minimum.traverse { minimum =>
            Validated.cond(
              test = value.length >= minimum,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.String.Minimum(minimum), length, hint = none))
            )
          } *> maximum.traverse { maximum =>
            Validated.cond(
              test = length <= maximum,
              (),
              Violations.rootNec(Violation(Constraint.Primitive.String.Maximum(maximum), length, hint = none))
            )
          } *> matches.traverse { pattern =>
            Validated.cond(
              test = pattern.matcher(value).matches(),
              (),
              Violations.rootNec(Violation(Constraint.Primitive.String.Matches(pattern), value, hint = none))
            )
          }).as(value)
    case schema => decoder.decode(schema, value)
