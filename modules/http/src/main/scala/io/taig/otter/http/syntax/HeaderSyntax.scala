package io.taig.otter.http.syntax

import io.taig.otter.Reference
import io.taig.otter.http.Header
import org.typelevel.ci.CIString

trait HeaderSyntax:
  def header[A](name: CIString, schema: => Header.Value[A]): Header[A] =
    Header.Root(name, schema = Reference.later(schema))

object HeaderSyntax extends HeaderSyntax
