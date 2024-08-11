package io.taig.otter.sample.api

import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.sample.api.schema.SessionApiSchema

trait Headers:
  def session: Header[SessionApiSchema] = header.authorization(SessionApiSchema.codec(prefix = "Bearer "))
