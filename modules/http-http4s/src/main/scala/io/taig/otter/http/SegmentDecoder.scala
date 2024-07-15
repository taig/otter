package io.taig.otter.http

import io.taig.otter.http.Segment.Parameter.Root
import io.taig.otter.http.Segment.Parameter.Transform
import io.taig.otter.http.Segment.Static
import io.taig.otter.ValueRequiredStringDecoder
import org.http4s.Uri as Http4sUri
import io.taig.otter.Decoder
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Constraint

object SegmentDecoder:
  def apply[A](segment: Segment[A], value: Http4sUri.Path.Segment): Decoder.Result[Option[String], A] =
    segment match
      case Segment.Parameter.Root(_, name, schema) =>
        Validated.cond(
          value.decoded() === name,
          (),
          Violations.rootNec(Violation(Constraint.Type(name), actual = value.decoded().some))
        ) *> ValueRequiredStringDecoder(schema, value.decoded())
      case Segment.Parameter.Transform(self, f, _) => SegmentDecoder(self, value).map(f)
      case Segment.Static(name) =>
        Validated.cond(
          value.decoded() === name,
          (),
          Violations.rootNec(Violation(Constraint.Type(name), actual = value.decoded().some))
        )
