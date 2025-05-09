package io.taig.otter.http

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.Violation
import io.taig.otter.Violations

object PathDataDecoder:
  object Remainders:
    def apply[A](path: Path[A], data: Path.Data): Validated[Violations, (Path.Data, A)] = path match
      case Path.Empty              => (data, ()).valid
      case Path.Modify(self, f, _) => apply(path = self, data).map(_.map(f))
      case Path.Root(segment) =>
        data.uncons match
          case Some((head, tail)) => SegmentDecoder(segment, value = head).tupleLeft(tail)
          case None               => Violations.rootNec(Violation.tpe(name = "segment", actual = Data.Null)).invalid
      case Path.Zip(left, right) =>
        apply(path = left, data).andThen: (data, a) =>
          apply(path = right, data).map(_.tupleLeft(a))
