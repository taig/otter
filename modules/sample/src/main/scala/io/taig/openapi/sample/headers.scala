package io.taig.openapi.sample

import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.http.Header
import io.taig.openapi.dsl.*
import io.taig.validation.Validation
import io.taig.validation.validations.*
import org.typelevel.ci.*

import java.util.UUID

object headers:
  val authorization: Header[UUID] =
    val Prefix = "Bearer"

    val bearer: Validation[String, String, String, UUID] = parser(Prefix) { value =>
      Option.when(value.startsWith(Prefix) && value.length > Prefix.length + 1)(value.substring(Prefix.length + 1))
    }.andThen(parser.uuid)

    header(ci"Authorization", string).ivalidate(bearer)(token => show"$Prefix $token")
