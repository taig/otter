package io.taig.otter.sample

import scala.collection.immutable.SortedMap

opaque type Inventory = SortedMap[Book, Int]
