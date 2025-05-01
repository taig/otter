package io.taig.otter.sample.api

import io.taig.otter.dsl.*
import io.taig.otter.sample.api.schema.SessionApiSchema

final case class AuthenticationApiSchema[A](session: Option[SessionApiSchema], payload: A)

object AuthenticationApiSchema:
  enum Error extends Throwable:
    case UserUnknown
    case Forbidden

//   val results: Results[AuthenticationApiSchema.Error] = (
//     result(code.unauthorized, json(error("userUnknown").as(Error.UserUnknown))) :+
//       result(code.forbidden, json(error("forbidden").as(Error.Forbidden)))
//   ).to
