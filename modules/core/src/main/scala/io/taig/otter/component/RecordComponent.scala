package io.taig.otter.component

import cats.Invariant
import io.taig.otter.Merge
import io.taig.otter.schema.RecordSchema
import io.taig.otter.syntax.InvariantSyntax.*

trait RecordComponent[Self[_], -Field[_]](using RecordSchema[Self, Field])
