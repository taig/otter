package io.taig.openapi

import io.circe.Json
import io.circe.jawn.JawnParser
import io.taig.openapi.http.Input
import io.taig.openapi.syntax as core
import io.taig.openapi.http.syntax as http
import io.taig.openapi.validation.{Constraint, Validation}

import java.nio.charset.StandardCharsets

object dsl:
  export core.*
  export http.{__, code, header, method, parameter, query}

  object input:
    export http.input.*

    val json: Input.Body.Singlepart.Strict[Json] =
      // TODO javascript parser
      val parser = new JawnParser()
      val validation: Validation[OpenApi, Array[Byte], Array[Byte], Json] =
        Validation.fromOptionNec(Constraint.parser("json".asOpenApi)) { bytes =>
          parser.parseByteArray(bytes).toOption
        }

      Input.Body.Singlepart.Strict.Bytes.ivalidate(validation)(_.noSpaces.getBytes(StandardCharsets.UTF_8))
