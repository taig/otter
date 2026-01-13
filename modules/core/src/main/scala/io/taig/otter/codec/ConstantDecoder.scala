package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constant
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Violation
import io.taig.data.Data

final class ConstantDecoder[F[_], A](decoder: Decoder[F, A], render: A => Data) extends Decoder[Constant.Read[F, *], A]:
  override def decode[B](schema: Constant.Read[F, B], value: A): Validated[Violations, B] = schema match
    case Constant.Modify(self, f, _)          => decode(schema = self, value).map(f)
    case self @ Constant.Root(schema, reference, eq, _) =>
      decoder
        .decode(schema.value, value)
        .andThen: a =>
          Validated
            .cond(
              eq.eqv(reference.value, a),
              a,
              Violation(
                constraint = Constraint.Generic.Equals(reference = self.value.value),
                actual = render(value),
                hint = none
              )
            )
            .leftMap(Violations.apply)
    case Constant.Read.Modify(self, f) => decode(schema = self, value).map(f)