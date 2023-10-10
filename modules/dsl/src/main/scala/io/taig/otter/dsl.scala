package io.taig.otter

import io.taig.otter.http.syntax as http
import io.taig.otter.http.circe.codecs as circe
import io.taig.otter.validation.validations

abstract class dsl
    extends DslBuilder[
      codecs & http & circe,
      validations,
      http.request,
      http.response & circe.response,
      http.input & circe.input,
      http.output & circe.output
    ](
      validations,
      http.request,
      new http.response with circe.response,
      new http.input with circe.input,
      new http.output with circe.output
    )
    with codecs
    with http
    with circe

object dsl extends dsl
