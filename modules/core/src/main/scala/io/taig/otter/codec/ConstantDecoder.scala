package io.taig.otter.codec

import cats.data.Validated
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Constant
import io.taig.otter.Violations
import io.taig.otter.Violation

final class ConstantDecoder[S[_], T](codec: Codec[S, T], render: T => Data) extends Decoder[Constant[S, *], T]:
  override def decode[A](schema: Constant[S, A], value: T): Validated[Violations, A] =
    decode(schema = schema.value, value)

  def decode[A](schema: Constant.Value[S, A], value: T): Validated[Violations, A] = schema match
    case Constant.Value.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Constant.Value.Root(schema, eq)   =>
      codec
        .decode(schema = schema.self.value, value)
        .andThen: a =>
          Validated
            .cond(
              test = eq.eqv(a, schema.value),
              (),
              Violation.fromConstraint(
                constraint = Constraint.Generic.Equals(
                  reference = render(codec.encode(schema = schema.self.value, schema.value))
                ),
                actual = render(value)
              )
            )
            .leftMap(Violations.rootNec)
