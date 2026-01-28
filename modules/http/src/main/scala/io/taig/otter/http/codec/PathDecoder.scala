package io.taig.otter.http.codec

import io.taig.data.Data
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Path
import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.validation.Violation
import io.taig.otter.Constraint

object PathDecoder extends Decoder.Remaining[Path.Read, Chain[String]]:
  override def decodeRemaining[A](
      path: Path.Read[A],
      values: Chain[String]
  ): Validated[Violations, (Chain[String], A)] = path match
    case Path.Empty                => (values, ()).valid
    case Path.Modify(self, f, _)   => decodeRemaining(path = self, values).map(_.map(f))
    case Path.Product(left, right) =>
      decodeRemaining(path = left, values).andThen: (values, a) =>
        decodeRemaining(path = right, values).map(_.tupleLeft(a))
    case Path.Read.Modify(self, f)      => decodeRemaining(path = self, values).map(_.map(f))
    case Path.Read.Product(left, right) =>
      decodeRemaining(path = left, values).andThen: (values, a) =>
        decodeRemaining(path = right, values).map(_.tupleLeft(a))
    case Path.Root(segment) =>
      values.uncons
        .toValid(Violations(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none)))
        .andThen((value, remainders) => HttpSegmentParser.decode(segment.value, value).tupleLeft(remainders))
