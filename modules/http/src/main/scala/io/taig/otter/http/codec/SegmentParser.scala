package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.Parser
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.validation.Violation
import io.taig.otter.Constraint
import io.taig.otter.Reference

object SegmentParser extends Parser[Segment.Read]:
  override def decode[A](segment: Segment.Read[A], value: String): Validated[Violations, A] = segment match
    case Segment.Dynamic.Modify(self, f, _)         => decode(segment = self, value).map(f)
    case Segment.Dynamic.Read.Modify(self, f)       => decode(segment = self, value).map(f)
    case Segment.Dynamic.Read.Root(name, parameter) => decode(name, parameter, value)
    case Segment.Dynamic.Root(name, parameter)      => decode(name, parameter, value)
    case Segment.Static.Root(name)                  => decode(name, value)
    case Segment.Static.Modify(self, f, _)          => decode(segment = self, value).map(f)
    case Segment.Static.Read.Modify(self, f)        => decode(segment = self, value).map(f)

  def decode(name: String, value: String): Validated[Violations, Unit] = Validated.cond(
    test = name === value,
    (),
    Violations(Violation(constraint = Constraint.Generic.Equals(reference = name), actual = value, hint = none))
  )

  def decode[A](
      name: String,
      parameter: Reference[Segment.Parameter.Read, A],
      value: String
  ): Validated[Violations, A] = decode(name, value) *>
    SegmentParameterParser.decode(parameter = parameter.value, value)
