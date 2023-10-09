package io.taig.otter.sample.api

import io.taig.otter.sample.data.Session
import io.taig.otter.dsl.*
import io.taig.otter.http.Results
import io.taig.otter.{codecs, Codec}

import scala.util.control.NoStackTrace

final case class Authentication[A](session: Option[Session], payload: A)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case Forbidden

  val codec: Results[Authentication.Error] =
    val userUnknown: Codec[Authentication.Error.UserUnknown.type] =
      codecs.error("userUnknown", dynamic.singleton(Authentication.Error.UserUnknown))
    val permissionDenied: Codec[Authentication.Error.Forbidden.type] =
      codecs.error("permissionDenied", dynamic.singleton(Authentication.Error.Forbidden))

    (result(code.unauthorized, output.json(userUnknown)) :+ result(code.forbidden, output.json(permissionDenied))).to
