package io.taig.openapi

object dsl:
  export io.taig.validation.syntax.*

  export syntax.*

  object validation:
    export io.taig.validation.validations.*
    export io.taig.validation.identifiers as identifier
    export io.taig.validation.constraints as constraint

  export schema.syntax.*
  export schema.schemas.*

  export http.syntax.*
  export http.schemas.*
