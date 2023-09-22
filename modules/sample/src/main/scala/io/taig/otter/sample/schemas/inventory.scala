package io.taig.otter.sample.schemas

import cats.implicits.*
import io.taig.otter.sample.Book
import io.taig.otter.{Collection, Record}
import io.taig.otter.schemas.*

import scala.collection.immutable.SortedMap

val inventory: Collection[Record, SortedMap[Book, Int]] =
  collection.sortedMap(book.main, int) { (book, quantity) =>
    field("book", book) :* field("quantity", quantity)
  }(using Ordering.by(_.isbn))
