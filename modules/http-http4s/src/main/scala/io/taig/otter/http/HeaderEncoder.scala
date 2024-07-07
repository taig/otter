package io.taig.otter.http

import cats.Id as Identity
import org.http4s.Header as Http4sHeader
import io.taig.otter.ValueStringEncoder
import io.taig.otter.http.Header.Writer.Root
import io.taig.otter.http.Header.Root
import io.taig.otter as Base
import org.typelevel.ci.CIString

object HeaderEncoder:
  def apply[A](header: Header.Writer[Identity, A], a: A): Option[Http4sHeader.Raw] = header match
    case Header.Root(name, schema)        => root(name, schema, a)
    case Header.Transform(self, _, f)     => transform(self, f, a)
    case Header.Writer.Root(name, schema) => root(name, schema, a)
    case Header.Writer.Transform(self, f) => transform(self, f, a)

  def root[A](name: CIString, schema: Base.Value.Writer[Identity, ?, A], a: A): Option[Http4sHeader.Raw] =
    ValueStringEncoder(schema, a).map(Http4sHeader.Raw(name, _))

  def transform[A, B](self: Header.Writer[Identity, A], f: B => A, b: B): Option[Http4sHeader.Raw] =
    HeaderEncoder(self, f(b))
