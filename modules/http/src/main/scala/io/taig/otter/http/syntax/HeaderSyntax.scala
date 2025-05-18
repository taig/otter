package io.taig.otter.http.syntax

import io.taig.otter.http.Http
import io.taig.otter.http.Header
import io.taig.otter.Metadata
import io.taig.otter.Reference
import org.typelevel.ci.CIString

trait HeaderSyntax:
  def header[A](name: CIString, schema: => Http.Header[A]): Header[A] = Header.Root(
    name,
    schema = Reference.later(schema),
    metadata = Metadata.Empty
  )

object HeaderSyntax extends HeaderSyntax
