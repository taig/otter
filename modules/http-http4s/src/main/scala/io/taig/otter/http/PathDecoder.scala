package io.taig.otter.http

import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Chain
import org.http4s.Uri as Http4sUri
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Constraint

object PathDecoder:
  def apply[A](path: Path[A], values: Chain[Http4sUri.Path.Segment]): Decoder.Result[Option[String], A] =
    ???

  def withRemainders[A](
      path: Path[A],
      values: Chain[Http4sUri.Path.Segment]
  ): Decoder.Result[Option[String], (Chain[Http4sUri.Path.Segment], A)] =
    path match
      case Path.Combine(left, right) =>
        withRemainders(left, values).andThen { case (remainders, a) =>
          withRemainders(right, remainders).map(_.tupleLeft(a))
        }
      case Path.Empty => (values, ()).valid
      case Path.One(segment) =>
        values.uncons match
          case Some((value, remainders)) => SegmentDecoder(segment, value).tupleLeft(remainders)
          case None => Violations.rootNec(Violation(Constraint.Type("string"), actual = "null".some)).invalid
      case Path.Transform(self, f, _) => withRemainders(self, values).map(_.map(f))
