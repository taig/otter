package io.taig.otter.codec

import cats.data.Validated
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Enumeration
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.validation.Violation

final class EnumerationDecoder[F[_], T](decoder: Decoder[F, T], encoder: Encoder[F, T], render: T => Data)
    extends Decoder[Enumeration.Read[F, *], T]:
  override def decode[A](schema: Enumeration.Read[F, A], value: T): Validated[Violations, A] = schema match
    case Enumeration.Modify(self, f, _)                => decode(schema = self, value).map(f)
    case schema @ Enumeration.Root(reference, mapping) =>
      decoder
        .decode(reference.value, value)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid:
              val references = schema.values.toList
                .map(mapping.apply)
                .map(encoder.encode(reference.value, _))
                .map(render)

              Violation(constraint = Constraint.Generic.OneOf(references), actual = render(value), hint = none)
            .leftMap(Violations.apply)
    case Enumeration.Read.Modify(self, f) => decode(schema = self, value).map(f)
