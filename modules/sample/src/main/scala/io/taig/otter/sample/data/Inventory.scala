package io.taig.otter.sample.data

import scala.collection.immutable.SortedMap

opaque type Inventory = SortedMap[Book, Int]
