package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Enumeration
import io.taig.validation.Violation
import io.taig.otter.Violations

final class EnumerationDecoder[-S[_], T](decoder: Decoder[S, T], encoder: Encoder[S, T], render: T => Data)
    extends Decoder[Enumeration[S, *], T]:
  override def decode[A](schema: Enumeration[S, A], value: T): Validated[Violations, A] = schema match
    case Enumeration.Modify(self, f, _)       => decode(schema = self, value).map(f)
    case Enumeration.Root(reference, mapping) =>
      decoder
        .decode(schema = reference.value, value)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid:
              Violation(
                constraint = Constraint.Generic.OneOf(
                  references =
                    schema.values.toList.map(mapping.apply).map(encoder.encode(schema = reference.value, _)).map(render)
                ),
                actual = render(value),
                hint = none
              )
            .leftMap(Violations.apply)

object EnumerationDecoder:
  def apply[S[_], A](decoder: Decoder[S, A], encoder: Encoder[S, A], render: A => Data): Decoder[Enumeration[S, *], A] =
    new EnumerationDecoder(decoder, encoder, render)
