package io.taig.otter.sample.api

import io.taig.otter.http as Http
import io.taig.otter.sample.api.schema.SessionApiSchema

trait Headers extends Http.Codecs:
  def session: Header[SessionApiSchema] = header.authorization(SessionApiSchema.codec(prefix = "Bearer "))
