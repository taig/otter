package io.taig.otter

import io.taig.otter.validation.validations
import io.taig.otter.schemas
import io.taig.otter.http.syntax as http

object dsl:
  export validation.validations

  export schemas.*

  export http.*
