package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Http
import io.taig.otter.http.Query

trait QuerySyntax:
  def query[A](name: String, schema: => Http.Query[A]): Query[A] = Query.Root(
    name,
    schema = Reference.later(schema),
    explode = true,
    style = Query.Style.Form,
    metadata = Metadata.Empty
  )

object QuerySyntax extends QuerySyntax
