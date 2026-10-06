package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*

enum TaggedTree:
  case End
  case Fork(children: List[TaggedTree])

object TaggedTree:
  lazy val schema: Json.Union[TaggedTree] = (
    branch.nested("end", TNil.to[TaggedTree.End.type]) :+
      branch.nested("fork", collection.list(TaggedTree.schema)).to[TaggedTree.Fork]
  ).to[TaggedTree].attr(Keys.name, "TaggedTree")

  lazy val merged: Json.Union[TaggedTree] = (
    branch.merged("end", RNil.to[TaggedTree.End.type]) :+
      branch.merged("fork", field("children", collection.list(TaggedTree.merged)).toRecord).to[TaggedTree.Fork]
  ).to[TaggedTree].attr(Keys.name, "MergedTree")
