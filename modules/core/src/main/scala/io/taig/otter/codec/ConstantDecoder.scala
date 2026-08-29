package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constant
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Violation

final class ConstantDecoder[F[-_, +_], T](decoder: Decoder[F, T], encoder: Encoder[F, T], render: T => Data)
    extends Decoder[[w, r] =>> Constant[F, w, r], T]:
  override def decode[R](schema: Constant[F, Nothing, R], value: T): Validated[Violations, R] =
    schema match
      case Constant.Modify(self, f, _)            => decode(self, value).map(f)
      case Constant.Root(reference, expected, eq) =>
        decoder
          .decode(reference.value, value)
          .andThen: a =>
            Validated
              .cond(
                eq.eqv(expected.value, a),
                (),
                Violation(
                  constraint = Constraint.Generic
                    .Equals(reference = render(encoder.encode(reference.value, expected.value))),
                  actual = render(value),
                  hint = none
                )
              )
              .leftMap(Violations.apply)
