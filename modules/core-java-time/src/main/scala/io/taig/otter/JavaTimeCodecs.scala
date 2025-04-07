package io.taig.otter

import cats.syntax.all.*

import java.time.Duration
import java.time.Instant
import java.time.format.DateTimeParseException

trait JavaTimeCodecs[Self[_]]:
  given primitive: Primitive.String.Syntax[Self]

  val duration: Self[Duration] = primitive.parser(name = "iso8601.duration")(value =>
    Either.catchOnly[DateTimeParseException](Duration.parse(value)).leftMap(_.getMessage)
  )(_.toString)

  val instant: Self[Instant] = primitive.parser(name = "iso8601.instant")(value =>
    Either.catchOnly[DateTimeParseException](Instant.parse(value)).leftMap(_.getMessage)
  )(_.toString)
