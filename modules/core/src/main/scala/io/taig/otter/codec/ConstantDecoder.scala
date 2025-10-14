package io.taig.otter.codec

import cats.syntax.all.*
import cats.data.Validated
import io.taig.data.Data
import io.taig.otter.Constant
import io.taig.otter.Constraint
import io.taig.otter.Violation
import io.taig.otter.Violations

final class ConstantDecoder[-S[_], T](decoder: Decoder[S, T], encoder: Encoder[S, T], render: T => Data)
    extends Decoder[Constant[S, *], T]:
  override def decode[A](schema: Constant[S, A], value: T): Validated[Violations, A] = schema match
    case Constant.Modify(self, f, _)          => decode(schema = self, value).map(f)
    case Constant.Root(schema, reference, eq) =>
      decoder
        .decode(schema = schema.value, value)
        .andThen: a =>
          Validated
            .cond(
              eq.eqv(reference, a),
              a,
              Violation(
                constraint = Constraint.Generic.Equals(
                  reference = render(encoder.encode(schema = schema.value, reference))
                ),
                actual = render(value),
                hint = none
              )
            )
            .leftMap(Violations.apply)

object ConstantDecoder:
  def apply[S[_], A](decoder: Decoder[S, A], encoder: Encoder[S, A], render: A => Data): Decoder[Constant[S, *], A] =
    new ConstantDecoder(decoder, encoder, render)
