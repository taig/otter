package io.taig.otter.json.circe

import io.taig.otter.Fix
import io.taig.otter.Schema

type JsonSchema[A] = Fix[Schema[*, A]]

object JsonSchema:
  type Reader[A] = Fix[Schema.Reader[*, A]]

  type Writer[A] = Fix[Schema.Writer[*, A]]
