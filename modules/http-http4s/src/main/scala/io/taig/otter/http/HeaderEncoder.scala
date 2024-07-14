package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import io.taig.otter.ValueStringEncoder
import io.taig.otter.http.*
import io.taig.otter.*

object HeaderEncoder:
  def apply[A](header: Header[A], a: A): Option[Http4sHeader.Raw] = header match
    case Header.Root(_, name, schema) => ValueStringEncoder(schema, a).map(Http4sHeader.Raw(name, _))
    case Header.Transform(self, _, f) => HeaderEncoder(self, f(a))
