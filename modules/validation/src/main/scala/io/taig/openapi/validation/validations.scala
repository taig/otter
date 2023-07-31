package io.taig.openapi.validation

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.OpenApi

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeParseException
import java.util.UUID

object validations:
  def minLength(reference: Int): Validation[String, Unit] =
    Validation(Constraint(identifier = "minLength", reference = OpenApi.fromInt(reference).some)): value =>
      Validated.condNec(
        value.length >= reference,
        (),
        OpenApi.fromInt(value.length).some
      )

  def maxLength(reference: Int): Validation[String, Unit] =
    Validation(Constraint(identifier = "maxLength", reference = OpenApi.fromInt(reference).some)): value =>
      Validated.condNec(
        value.length <= reference,
        (),
        OpenApi.fromInt(value.length).some
      )

  val uuid: Validation[String, UUID] = Validation.parse("uuid"): value =>
    try UUID.fromString(value).some
    catch case _: IllegalArgumentException => none

  val date: Validation[String, LocalDate] = Validation.parse("date"): value =>
    try LocalDate.parse(value).some
    catch case _: DateTimeParseException => none

  val dateTime: Validation[String, LocalDateTime] = Validation.parse("date-time"): value =>
    try LocalDateTime.parse(value).some
    catch case _: DateTimeParseException => none
