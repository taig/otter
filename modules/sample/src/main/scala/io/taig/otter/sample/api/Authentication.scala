package io.taig.otter.sample.api

import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.http.Results
import io.taig.otter.sample.Dsl.*

import scala.util.control.NoStackTrace

final case class Authentication[A](session: Option[SessionApiSchema], payload: A)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case Forbidden

  val codec: Results[Authentication.Error] =
    val userUnknown: Codec[Authentication.Error.UserUnknown.type] =
      error("userUnknown", singleton(Authentication.Error.UserUnknown))
    val permissionDenied: Codec[Authentication.Error.Forbidden.type] =
      error("permissionDenied", singleton(Authentication.Error.Forbidden))

    ???

    // (result(code.unauthorized, output.json(userUnknown)) :+ result(code.forbidden, output.json(permissionDenied))).to
