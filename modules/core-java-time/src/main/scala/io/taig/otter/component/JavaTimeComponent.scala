package io.taig.otter.component

import io.taig.otter.operation.StringOperation
import java.time.Instant
import cats.syntax.all.*
import java.time.format.DateTimeParseException
import java.time.Duration

trait JavaTimeComponent[+Self[_]](using operation: StringOperation[Self]):
  val duration: Self[Duration] = operation.parser(
    name = "iso8601[duration]",
    decode = value => Either.catchOnly[DateTimeParseException](Duration.parse(value)).leftMap(_.getMessage),
    encode = _.toString
  )

  val instant: Self[Instant] = operation.parser(
    name = "iso8601[instant]",
    decode = value => Either.catchOnly[DateTimeParseException](Instant.parse(value)).leftMap(_.getMessage),
    encode = _.toString
  )
