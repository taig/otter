package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Parameter

trait ParameterSyntax:
  def parameter[A](name: String, schema: => Parameter.Schema[A]): Parameter[A] =
    Parameter(
      value = Parameter.Value.Root(name, schema = Reference.later(schema), style = Parameter.Style.Simple),
      metadata = Metadata.Empty
    )

object ParameterSyntax extends ParameterSyntax
