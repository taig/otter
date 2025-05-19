package io.taig.otter.component

import io.taig.otter.schema.RecordSchema
import io.taig.otter.syntax.InvariantSyntax.*
import io.taig.otter.Merge
import scala.annotation.targetName

trait RecordComponent[Self[_], -Field[_]](using self: RecordSchema[Self, Field]):
  export self.{:*, toRecord}
