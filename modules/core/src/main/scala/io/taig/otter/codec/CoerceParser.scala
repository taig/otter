package io.taig.otter.codec

import io.taig.otter.Coerce
import cats.data.Validated
import io.taig.otter.Violations

final class CoerceParser[F[_]](parser: Parser[F]) extends Parser[Coerce.Read[F, *]]:
  override def decode[A](schema: Coerce.Read[F, A], a: String): Validated[Violations, A] = schema match
    case Coerce.Modify(self, f, _)   => decode(self, a).map(f)
    case Coerce.Root(schema)         => parser.decode(schema.value, a)
    case Coerce.Read.Modify(self, f) => decode(self, a).map(f)
