package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constant
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Violation

final class ConstantDecoder[S[_], T](decoder: Decoder[S, T], encoder: Encoder[S, T], render: T => Data)
    extends Decoder[Constant[S, *], T]:
  override def decode[A](schema: Constant[S, A], value: T): Validated[Violations, A] = schema match
    case Constant.Modify(self, f, _)          => decode(schema = self, value).map(f)
    case Constant.Root(schema, reference, eq) =>
      decoder
        .decode(schema.value, value)
        .andThen: a =>
          Validated
            .cond(
              eq.eqv(reference.value, a),
              a,
              Violation(
                constraint = Constraint.Generic.Equals(
                  reference = render(encoder.encode(schema.value, reference.value))
                ),
                actual = render(value),
                hint = none
              )
            )
            .leftMap(Violations.apply)
