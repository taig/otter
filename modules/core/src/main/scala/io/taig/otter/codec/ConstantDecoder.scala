package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Constant
import io.taig.otter.Data
import io.taig.otter.Violation
import io.taig.otter.Violations

final class ConstantDecoder[S[_], T](codec: Codec[S, T], render: T => Data.Any) extends Decoder[Constant[S, *], T]:
  override def decode[A](schema: Constant[S, A], value: T): Validated[Violations, A] = schema match
    case Constant.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Constant.Root(schema, eq, _) =>
      codec
        .decode(schema = schema.self.value, value)
        .andThen: a =>
          Validated
            .cond(
              test = eq.eqv(a, schema.value),
              (),
              Violation.equal(reference = render(codec.encode(schema = schema.self.value, schema.value)), render(value))
            )
            .leftMap(Violations.rootNec)
