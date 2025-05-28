package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Query

trait QuerySyntax:
  def query[A](name: String, schema: => Query.Schema[A]): Query[A] = Query(
    value = Query.Value.Root(
      name,
      schema = Reference.later(schema),
      explode = true,
      style = Query.Style.Form
    ),
    metadata = Metadata.Empty
  )

object QuerySyntax extends QuerySyntax
