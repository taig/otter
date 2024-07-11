package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import io.taig.otter.ValueStringEncoder
import io.taig.otter.http as Base
import io.taig.otter.http.Plain.*
import org.typelevel.ci.CIString

object HeaderEncoder:
  def apply[A](header: Header.Writer[A], a: A): Option[Http4sHeader.Raw] = header match
    case Base.Header.Root(name, schema)        => root(name, schema, a)
    case Base.Header.Transform(self, _, f)     => transform(self, f, a)
    case Base.Header.Writer.Root(name, schema) => root(name, schema, a)
    case Base.Header.Writer.Transform(self, f) => transform(self, f, a)

  def root[A](name: CIString, schema: Value.Writer.Via[String, A], a: A): Option[Http4sHeader.Raw] =
    ValueStringEncoder(schema, a).map(Http4sHeader.Raw(name, _))

  def transform[A, B](self: Header.Writer[A], f: B => A, b: B): Option[Http4sHeader.Raw] =
    HeaderEncoder(self, f(b))
