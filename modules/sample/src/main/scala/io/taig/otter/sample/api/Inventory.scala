package io.taig.otter.sample.api

import scala.collection.immutable.SortedMap

opaque type Inventory = SortedMap[Book, Int]
