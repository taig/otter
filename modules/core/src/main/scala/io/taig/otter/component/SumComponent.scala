package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.schema.SumSchema

import scala.annotation.targetName

trait SumComponent[Self[_], -Branch[_]](using self: SumSchema[Self, Branch]):
  export self.{:+, |, orElse, toSum}
