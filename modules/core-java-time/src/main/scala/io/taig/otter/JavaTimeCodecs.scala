package io.taig.otter

import cats.syntax.all.*

import java.time.Duration
import java.time.Instant
import java.time.format.DateTimeParseException

abstract class JavaTimeCodecs[S[_]](codecs: Primitives.Strings[S]):
  import codecs.*

  val duration: S[Duration] = parser(name = "iso8601.duration")(value =>
    Either.catchOnly[DateTimeParseException](Duration.parse(value)).leftMap(_.getMessage)
  )(_.toString)

  val instant: S[Instant] = parser(name = "iso8601.instant")(value =>
    Either.catchOnly[DateTimeParseException](Instant.parse(value)).leftMap(_.getMessage)
  )(_.toString)
