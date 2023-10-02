package io.taig.otter.sample.api.codecs

import io.taig.otter.dsl.*
import io.taig.otter.http.Results
import io.taig.otter.sample.api.endpoints.Authentication
import io.taig.otter.{codecs, Codec}

object authentication:
  val error: Results[Authentication.Error] =
    val userUnknown: Codec[Authentication.Error.UserUnknown.type] =
      codecs.error("userUnknown", dynamic.singleton(Authentication.Error.UserUnknown))
    val permissionDenied: Codec[Authentication.Error.Forbidden.type] =
      codecs.error("permissionDenied", dynamic.singleton(Authentication.Error.Forbidden))

    (result(code.unauthorized, output.json(userUnknown)) :+ result(code.forbidden, output.json(permissionDenied))).to
