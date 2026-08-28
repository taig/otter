package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constant
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Violation

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class ConstantDecoder[F[-_, +_], T](decoder: Decoder[F, T], encoder: Encoder[F, T], render: T => Data)
    extends Decoder[[w, r] =>> Constant[F, w, r], T]:
  override def decode[R](schema: Constant[F, Nothing, R], value: T): Validated[Violations, R] =
    (schema: @unchecked) match
      case schema: Constant.Modify[F, ?, ?, ?, R] => decode(schema.self, value).map(schema.f)
      case schema: Constant.Root[F, ?]            =>
        decoder
          .decode(schema.reference.value, value)
          .andThen: a =>
            Validated
              .cond(
                schema.eq.eqv(schema.value.value, a),
                (),
                Violation(
                  constraint = Constraint.Generic
                    .Equals(reference = render(encoder.encode(schema.reference.value, schema.value.value))),
                  actual = render(value),
                  hint = none
                )
              )
              .leftMap(Violations.apply)
          .map(_.asInstanceOf[R])
