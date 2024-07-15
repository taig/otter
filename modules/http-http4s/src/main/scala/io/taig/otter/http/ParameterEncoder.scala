package io.taig.otter.http

import io.taig.otter.http.Parameter.Root
import io.taig.otter.http.Parameter.Transform
import io.taig.otter.ValueStringEncoder

object ParameterEncoder:
  def apply[A](query: Parameter[A], a: A): (String, Option[String]) = query match
    case Parameter.Root(_, name, schema) => (name, ValueStringEncoder(schema, a))
    case Parameter.Transform(self, _, f) => ParameterEncoder(self, f(a))
