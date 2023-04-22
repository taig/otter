package io.taig.openapi.authentication

import io.taig.openapi.http.Output
import io.taig.openapi.http.schemas.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.schemas.*

object schemas:
  object authentication:
    val errors: Output.Results[Authentication.Error] = (
      result(code.unauthorized, error("unauthorized", singleton(Authentication.Error.UserUnknown))) +
        result(code.forbidden, error("forbidden", singleton(Authentication.Error.AccessForbidden)))
    ).gimap
