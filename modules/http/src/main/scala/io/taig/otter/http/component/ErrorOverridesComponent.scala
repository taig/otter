package io.taig.otter.http.component

import io.taig.otter.http.ErrorOverrides
import io.taig.otter.http.Failure
import io.taig.otter.http.Response

/** Named endpoint-local replacements; omitted categories inherit the consumer's policy. */
trait ErrorOverridesComponent:
  def apply[
      S1[-_, +_],
      E1,
      S2[-_, +_],
      E2,
      S3[-_, +_],
      E3,
      S4[-_, +_],
      E4,
      S5[-_, +_],
      E5,
      S6[-_, +_],
      E6,
      S7[-_, +_],
      E7,
      S8[-_, +_],
      E8
  ](
      envelope: Option[Response.Schema[S1, Failure, E1]] = None,
      syntax: Option[Response.Schema[S2, Failure, E2]] = None,
      contentType: Option[Response.Schema[S3, Failure, E3]] = None,
      validation: Option[Response.Schema[S4, Failure, E4]] = None,
      entityRead: Option[Response.Schema[S5, Failure, E5]] = None,
      encoding: Option[Response.Schema[S6, Failure, E6]] = None,
      status: Option[Response.Schema[S7, Failure, E7]] = None,
      unexpected: Option[Response.Schema[S8, Failure, E8]] = None
  ): ErrorOverrides[
    [w, r] =>> S1[w, r] | S2[w, r] | S3[w, r] | S4[w, r] | S5[w, r] | S6[w, r] | S7[w, r] | S8[w, r],
    E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8
  ] = ErrorOverrides(
    envelope = envelope,
    syntax = syntax,
    contentType = contentType,
    validation = validation,
    entityRead = entityRead,
    encoding = encoding,
    status = status,
    unexpected = unexpected
  )

object ErrorOverridesComponent extends ErrorOverridesComponent
