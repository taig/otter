package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import org.http4s.Uri as Http4sUri

object Http4sPathDecoder:
  // def apply[A](path: Path[A], value: Http4sUri.Path): Validated[Violations, A] =
  //   apply(path, segments = value.segments).andThen: (segments, a) =>
  //     Validated.cond(
  //       test = segments.isEmpty,
  //       a,
  //       Violations.rootNec(Violation.equal(reference = "/", actual = segments.map(_.encoded).mkString_("/")))
  //     )

  def apply[A](
      path: Path[A],
      segments: List[Http4sUri.Path.Segment]
  ): Validated[Violations, (List[Http4sUri.Path.Segment], A)] = path match
    case Path.Empty              => (segments, ()).valid
    case Path.Modify(self, f, g) => apply(path = self, segments).map(_.map(f))
    case Path.Root(segment) =>
      segments match
        case head :: tail => Http4sSegmentDecoder(segment, value = head.encoded.some).tupleLeft(tail)
        case Nil          => Http4sSegmentDecoder(segment, value = none).tupleLeft(Nil)
    case Path.Zip(left, right) =>
      apply(path = left, segments).andThen: (segments, a) =>
        apply(path = right, segments).map((segments, b) => (segments, (a, b)))
