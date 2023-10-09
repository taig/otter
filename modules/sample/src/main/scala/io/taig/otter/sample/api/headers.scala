package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.dsl.*
import io.taig.otter.http.Header
import io.taig.otter.sample.data.Session
import io.taig.otter.validation.{validations, Validation}
import org.typelevel.ci.*

import java.util.UUID

object headers:
  val authorization: Header[String] = header(ci"Authorization", string)

  val authorizationBearer: Header[String] =
    val Prefix = "Bearer"

    val bearer: Validation[String, String] = validations.parse(Prefix) { value =>
      Option.when(value.startsWith(Prefix) && value.length > Prefix.length + 1)(value.substring(Prefix.length + 1))
    }

    authorization.ivalidate(bearer)(token => show"$Prefix $token")

  val authorizationBearerUuid: Header[UUID] = authorizationBearer.ivalidate(validations.uuid)(_.toString)

  val session: Header[Session] = authorizationBearerUuid.imap(Session.fromUUID)(_.toUUID)
