package io.taig.otter.sample.api

import io.taig.otter.http.Results
import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.sample.api.schema.SessionApiSchema

final case class AuthenticationApiSchema[A](session: Option[SessionApiSchema], payload: A)

object AuthenticationApiSchema:
  enum Error extends Throwable:
    case UserUnknown
    case Forbidden

  val codec: Results[AuthenticationApiSchema.Error] = ???
  // val userUnknown: Codec[AuthenticationApiSchema.Error.UserUnknown.type] =
  //   error("userUnknown", singleton(AuthenticationApiSchema.Error.UserUnknown))
  // val permissionDenied: Codec[AuthenticationApiSchema.Error.Forbidden.type] =
  //   error("permissionDenied", singleton(AuthenticationApiSchema.Error.Forbidden))

  // (result(code.unauthorized, json(userUnknown)) :+ result(code.forbidden, json(permissionDenied))).to
