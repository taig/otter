package io.taig.otter.sample.api

import io.taig.otter.http.Dsl.*
import io.taig.otter.sample.api.schema.SessionApiSchema

trait Headers:
  val session: Header[SessionApiSchema] = header.authorization(SessionApiSchema.codec(prefix = "Bearer "))

object Headers extends Headers
