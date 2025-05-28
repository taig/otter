package io.taig.otter.http.syntax

import io.taig.otter.Reference
import io.taig.otter.http.Header
import org.typelevel.ci.CIString
import io.taig.otter.Metadata

trait HeaderSyntax:
  def header[A](name: CIString, schema: => Header.Schema[A]): Header[A] =
    Header(value = Header.Value.Root(name, schema = Reference.later(schema)), metadata = Metadata.Empty)

object HeaderSyntax extends HeaderSyntax
