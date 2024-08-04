package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.Header
import io.taig.otter.sample.data.Session
import io.taig.otter.sample.Dsl.*
import org.typelevel.ci.*

import java.util.UUID

object headers:
  val session: Header[Session] =
    val Prefix = "Bearer"
    val codec = parser("bearer-session") { value =>
      Option
        .when(value.startsWith(Prefix) && value.length > Prefix.length + 1)(value.substring(Prefix.length + 1))
        .flatMap(value =>
          try UUID.fromString(value).some
          catch { case _: IllegalArgumentException => none }
        )
    }(uuid => s"Bearer $uuid").imap(Session.apply)(_.toUUID)

    header.authorization(codec)
