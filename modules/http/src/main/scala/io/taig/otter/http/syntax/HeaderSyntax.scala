package io.taig.otter.http.syntax

import io.taig.otter.Enrichment
import io.taig.otter.Reference
import io.taig.otter.http.Header
import org.typelevel.ci.CIString

trait HeaderSyntax:
  def header[A](name: CIString, schema: => Header.Schema[A]): Header[A] =
    Header(Enrichment(Header.Value.Root(name, schema = Reference.later(schema))))

object HeaderSyntax extends HeaderSyntax
