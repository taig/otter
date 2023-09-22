package io.taig.otter.sample.schemas

import cats.implicits.*
import io.taig.otter.sample.Book
import io.taig.otter.Schema
import io.taig.otter.schemas.*

import scala.collection.immutable.SortedMap

val inventory: Schema.Collection[Schema.Record, SortedMap[Book, Int]] =
  collection.sortedMap(book.main, int) { (book, quantity) =>
    field("book", book) :* field("quantity", quantity)
  }(using Ordering.by(_.isbn))
