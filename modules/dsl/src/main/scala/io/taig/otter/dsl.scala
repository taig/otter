package io.taig.otter

import io.taig.otter.validation.validations
import io.taig.otter.syntax as core
import io.taig.otter.http.syntax as http

object dsl:
  export core.*

  export validation.validations

  export http.*
