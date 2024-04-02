package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity

object OpenApi extends Dsl:
  self =>

  abstract class Field[S, B] {
    def value: B
    def update(f: B => B): S
  }
