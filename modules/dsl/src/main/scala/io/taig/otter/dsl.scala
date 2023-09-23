package io.taig.otter

import io.taig.otter.validation.validations
import io.taig.otter.circe.syntax as circe
import io.taig.otter.http.syntax as http

object dsl:
  export validation.validations

  export schemas.*

  export http.{input as _, output as _, *}

  export circe.{input as _, output as _, *}

  object input:
    export http.input.*
    export circe.input.*

  object output:
    export http.output.*
    export circe.output.*
