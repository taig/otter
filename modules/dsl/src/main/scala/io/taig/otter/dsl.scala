package io.taig.otter

import io.taig.otter.circe.syntax as circe
import io.taig.otter.http.syntax as http
import io.taig.otter.validation.validations

object dsl:
  export validation.validations

  export codecs.*

  export http.{input as _, output as _, *}

  export circe.{input as _, output as _, *}

  object input:
    export http.input.*
    export circe.input.*

  object output:
    export http.output.*
    export circe.output.*
