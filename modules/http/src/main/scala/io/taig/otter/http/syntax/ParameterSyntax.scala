package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Parameter

trait ParameterSyntax:
  def parameter[A](name: String, schema: => Parameter.Value[A]): Parameter[A] =
    Parameter.Root(name, schema = Reference.later(schema), style = Parameter.Style.Simple, metadata = Metadata.Empty)

object ParameterSyntax extends ParameterSyntax
