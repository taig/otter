package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.otter.Violation

object SegmentDecoder:
  def apply[A](segment: Segment[A], value: String): Validated[Violations, A] = segment match
    case Segment.Static(name, _) =>
      Validated.cond(
        test = value === name,
        (),
        Violations.rootNec(Violation.equal(reference = name, actual = value))
      )
    case Segment.Parameter(name, codec, metadata) =>
      val explode = metadata.get(HttpKeys.explode).getOrElse(false) // TODO proper default
      val style = metadata
        .get(HttpKeys.style)
        .collect { case style: Header.Style => style }
        .getOrElse(Header.Style.Simple)

      HttpSegmentParser(explode, style)(name, codec = codec.value, value)
    case Segment.Modify(self, f, _) => apply(segment = self, value).map(f)
