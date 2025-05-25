package io.taig.otter.http.syntax

import io.taig.otter.http.component.BodyComponent
import io.taig.otter.http.Types

trait BodySyntax extends BodyComponent[Types.Body]

object BodySyntax extends BodySyntax
