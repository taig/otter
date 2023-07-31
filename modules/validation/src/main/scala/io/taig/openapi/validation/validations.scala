package io.taig.openapi.validation

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.OpenApi

object validations:
  def minLength(reference: Int): Validation[String, Unit] =
    Validation(Constraint(identifier = "minLength", reference = OpenApi.fromInt(reference).some)): value =>
      Validated.condNel(
        value.length >= reference,
        (),
        OpenApi.fromInt(value.length).some
      )

  def maxLength(reference: Int): Validation[String, Unit] =
    Validation(Constraint(identifier = "maxLength", reference = OpenApi.fromInt(reference).some)): value =>
      Validated.condNel(
        value.length <= reference,
        (),
        OpenApi.fromInt(value.length).some
      )
