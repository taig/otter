package io.taig.otter

import io.taig.otter.http.syntax as http
import io.taig.otter.http.circe.codecs as circe
import io.taig.otter.validation.validations

object dsl:
  export validation.validations

  export codecs.*

  export http.{input as _, output as _, response as _, *}

  export circe.{input as _, output as _, response as _, *}

  object response:
    export http.response.*
    export circe.response.*

  object input:
    export http.input.*
    export circe.input.*

  object output:
    export http.output.*
    export circe.output.*
