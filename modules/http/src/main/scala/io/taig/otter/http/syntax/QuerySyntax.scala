package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Query
import io.taig.otter.Enrichment

trait QuerySyntax:
  def query[A](name: String, schema: => Query.Schema[A]): Query[A] = Enrichment(
    Query.Value.Root(
      name,
      schema = Reference.later(schema),
      explode = true,
      style = Query.Style.Form
    )
  )

object QuerySyntax extends QuerySyntax
