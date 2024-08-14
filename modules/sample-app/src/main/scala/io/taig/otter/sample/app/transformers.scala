package io.taig.otter.sample.app

import io.taig.otter.sample.Session
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.IsbnApiSchema
import io.taig.otter.json.*
import io.taig.otter.sample.Isbn
import io.github.arainko.ducktape.*
import io.taig.otter.Data
import io.circe.JsonObject

object transformers:
  given Transformer[Isbn, IsbnApiSchema] = isbn => IsbnApiSchema.unsafe(isbn.toLong)

  given Transformer[IsbnApiSchema, Isbn] = isbn => Isbn(isbn.toLong)

  given Transformer[JsonObject, Data.Object[?]] = toDataObject

  given Transformer[Data.Object[?], JsonObject] = fromData

  given Transformer[Session, SessionApiSchema] = session => SessionApiSchema(session.toUUID)

  given Transformer[SessionApiSchema, Session] = session => Session(session.toUUID)
