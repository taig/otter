package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Coerce
import io.taig.otter.Coerce.Modify
import io.taig.otter.Coerce.Root
import io.taig.otter.Violation

final class CoerceParser[-S[_]](parser: Parser[S]) extends Parser[Coerce[S, *]]:
  override def parse[A](schema: Coerce[S, A], value: String): Validated[Violation, A] = schema match
    case Coerce.Modify(self, f, _) => parse(schema = self, value).map(f)
    case Coerce.Root(schema)       => parser.parse(schema = schema.value, value)

object CoerceParser:
  def apply[S[_]](parser: Parser[S]): Parser[Coerce[S, *]] = new CoerceParser(parser)
