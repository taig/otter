package io.taig.otter.openapi

import io.taig.otter.http as Http

trait Dsl extends Http.Dsl, Keys, Syntax

object Dsl extends Dsl
