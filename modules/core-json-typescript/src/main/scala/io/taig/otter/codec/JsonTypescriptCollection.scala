package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Json

/** What a collection schema was built with, whatever it has been wrapped in since.
  *
  * Read by both halves of a target: the value it emits and the type it declares have to agree about whether a
  * collection is ever empty, and a declaration that disagrees with its own value does not compile.
  */
object JsonTypescriptCollection:
  def constraints[W, R](schema: Collection[Json.Node, W, R]): Chain[Constraint] = schema match
    case Collection.Chained(_, validation) => validation.constraints
    case Collection.Indexed(_, validation) => validation.constraints
    case Collection.Linked(_, validation)  => validation.constraints
    case Collection.Modify(self, _, _)     => constraints(self)
