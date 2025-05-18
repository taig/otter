package io.taig.otter.http.syntax

import io.taig.otter.http.Http
import io.taig.otter.http.Parameter
import io.taig.otter.Reference
import io.taig.otter.Metadata

trait ParameterSyntax:
  def parameter[A](name: String, schema: => Http.Parameter[A]): Parameter[A] =
    Parameter.Root(name, schema = Reference.later(schema), style = Parameter.Style.Simple, metadata = Metadata.Empty)

object ParameterSyntax extends ParameterSyntax
