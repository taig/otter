package io.taig.otter.codec

import io.taig.otter.Enumeration
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Data

final class EnumerationDecoder[S[_], T](codec: Codec[S, T], render: T => Data.Primitive)
    extends Decoder[Enumeration[S, *], T]:
  override def decode[A](schema: Enumeration[S, A], value: T): Validated[Violations, A] = schema match
    case Enumeration.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Enumeration.Root(reference, mapping, _) =>
      codec
        .decode(schema = reference.value, value)
        .andThen: a =>
          mapping
            .unapply(a)
            .toValid:
              val values = schema.values.map(mapping.apply).map(codec.encode(reference.value, _))
              Violation.oneOf(values = values.map(render).toList, actual = render(value))
            .leftMap(Violations.rootNec)
