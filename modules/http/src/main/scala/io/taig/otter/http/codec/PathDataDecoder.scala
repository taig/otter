package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Path
import io.taig.otter.Violation
import io.taig.otter.Constraint

object PathDataDecoder extends Decoder.Remaining[Path, Path.Data]:
  override def decodeRemaining[A](schema: Path[A], value: Path.Data): Validated[Violations, (Path.Data, A)] =
    decodeRemaining(schema = schema.value, value)

  def decodeRemaining[A](schema: Path.Value[A], value: Path.Data): Validated[Violations, (Path.Data, A)] = schema match
    case Path.Value.Empty              => (value, ()).valid
    case Path.Value.Modify(self, f, _) => decodeRemaining(schema = self, value).map(_.map(f))
    case Path.Value.Static(name)       =>
      value.uncons match
        case Some((head, tail)) =>
          Validated
            .cond(
              test = head === name,
              (tail, ()),
              Violation.fromConstraint(constraint = Constraint.Generic.Equals(reference = name), actual = head)
            )
            .leftMap(Violations.rootNec)
        case None =>
          val violation =
            Violation.fromConstraint(constraint = Constraint.Generic.Equals(reference = name), actual = Data.Null)
          Violations.rootNec(violation).invalid
    case Path.Value.Root(segment) =>
      value.uncons match
        case Some((head, tail)) => ParameterParser.decode(segment, value = head).tupleLeft(tail)
        case None               =>
          val violation =
            Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "parameter"), actual = Data.Null)
          Violations.rootNec(violation).invalid
    case Path.Value.Zip(left, right) =>
      decodeRemaining(schema = left, value).andThen: (value, a) =>
        decodeRemaining(schema = right, value).map(_.tupleLeft(a))
