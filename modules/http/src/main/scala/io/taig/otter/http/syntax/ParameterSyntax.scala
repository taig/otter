package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Parameter
import io.taig.otter.Enrichment

trait ParameterSyntax:
  def parameter[A](name: String, schema: => Parameter.Schema[A]): Parameter[A] =
    Parameter(Enrichment(Parameter.Value.Root(name, schema = Reference.later(schema), style = Parameter.Style.Simple)))

object ParameterSyntax extends ParameterSyntax
