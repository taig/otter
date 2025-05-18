package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Path

object PathDataDecoder extends Decoder.Remainding[Path, Path.Data]:
  def decodeRemainding[A](path: Path[A], data: Path.Data): Validated[Violations, (Path.Data, A)] = path match
    case Path.Empty              => (data, ()).valid
    case Path.Modify(self, f, _) => decodeRemainding(path = self, data).map(_.map(f))
    case Path.Static(name) =>
      data.uncons match
        case Some((head, tail)) =>
          Validated.cond(
            test = head === name,
            (tail, ()),
            Violations.rootNec(Violation.equal(reference = name, actual = head))
          )
        case None => Violations.rootNec(Violation.equal(reference = name, actual = Data.Null)).invalid
    case Path.Root(segment) =>
      data.uncons match
        case Some((head, tail)) => ParameterParser.decode(segment, value = head).tupleLeft(tail)
        case None               => Violations.rootNec(Violation.tpe(name = "parameter", actual = Data.Null)).invalid
    case Path.Zip(left, right) =>
      decodeRemainding(path = left, data).andThen: (data, a) =>
        decodeRemainding(path = right, data).map(_.tupleLeft(a))
