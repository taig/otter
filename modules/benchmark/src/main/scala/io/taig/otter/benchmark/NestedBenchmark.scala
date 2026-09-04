package io.taig.otter.benchmark

import io.taig.otter.fixture.Tree
import io.taig.otter.fixture.json

/** A schema that recurses, so whatever a record costs per node is paid once per node of the tree. */
class NestedBenchmark extends JsonBenchmark(json.tree, NestedBenchmark.Value)

object NestedBenchmark:
  /** Four levels, branching three ways: forty values, and a record read for every one of them. */
  private def grow(depth: Int): Tree =
    Tree(depth, if depth <= 0 then Nil else List.fill(3)(grow(depth - 1)))

  val Value: Tree = NestedBenchmark.grow(3)
