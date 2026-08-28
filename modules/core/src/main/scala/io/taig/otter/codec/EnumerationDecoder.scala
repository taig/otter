package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Enumeration
import io.taig.otter.Violations
import io.taig.validation.Violation

final class EnumerationDecoder[F[-_, +_], T](decoder: Decoder[F, T], encoder: Encoder[F, T], render: T => Data)
    extends Decoder[[w, r] =>> Enumeration[F, w, r], T]:
  override def decode[R](schema: Enumeration[F, Nothing, R], value: T): Validated[Violations, R] =
    (schema: @unchecked) match
      case schema: Enumeration.Modify[F, ?, ?, ?, R] => decode(schema.self, value).map(schema.f)
      case schema: Enumeration.Root[F, a, R]         =>
        decoder
          .decode(schema.reference.value, value)
          .andThen: decoded =>
            schema.mapping
              .prj(decoded)
              .toValid:
                val references = schema.mapping.values.toList
                  .map(schema.mapping.inj)
                  .map(encoder.encode(schema.reference.value, _))
                  .map(render)
                Violation(constraint = Constraint.Generic.OneOf(references), actual = render(value), hint = none)
              .leftMap(Violations.apply)
