package io.taig.otter.sample.api.schemas

import io.taig.otter.http.Results
import io.taig.otter.dsl.*
import io.taig.otter.{schemas, Schema}
import io.taig.otter.sample.api.endpoints.Authentication

object authentication:
  val error: Results[Authentication.Error] =
    val userUnknown: Schema[Authentication.Error.UserUnknown.type] =
      schemas.error("userUnknown", dynamic.singleton(Authentication.Error.UserUnknown))
    val permissionDenied: Schema[Authentication.Error.Forbidden.type] =
      schemas.error("permissionDenied", dynamic.singleton(Authentication.Error.Forbidden))

    (result(code.unauthorized, output.json(userUnknown)) :+ result(code.forbidden, output.json(permissionDenied))).to
