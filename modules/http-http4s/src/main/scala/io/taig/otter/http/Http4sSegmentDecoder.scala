package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.Violation
import io.taig.otter.Violations

object Http4sSegmentDecoder:
  def apply[A](segment: Segment[A], value: Option[String]): Validated[Violations, A] =
    segment match
      case Segment.Static(name, _) =>
        Validated.cond(
          test = value.contains_(name),
          (),
          Violations.rootNec(Violation.equal(reference = name, actual = value.getOrElse(Data.Null)))
        )
      case Segment.Parameter(name, codec, metadata) =>
        val explode = metadata.get(HttpKeys.explode).getOrElse(false) // TODO proper default
        val style = metadata
          .get(HttpKeys.style)
          .collect { case style: Header.Style => style }
          .getOrElse(Header.Style.Simple)

        value
          .toValid(Violations.rootNec(Violation.tpe(name = "string", actual = Data.Null)))
          .andThen(value => HttpSegmentParser(explode, style)(name, codec = codec.value, value))
      case Segment.Modify(self, f, _) => apply(segment = self, value).map(f)
