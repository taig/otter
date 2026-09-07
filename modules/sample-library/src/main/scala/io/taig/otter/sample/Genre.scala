package io.taig.otter.sample

import cats.Order

/** What a book is, as a closed set.
  *
  * A Scala 3 `enum` and an `enumeration` schema over it, which is the pairing that keeps the wire spelling and the type
  * from drifting: the schema matches on every case, so adding one here is a compile error there rather than a value
  * silently failing to read.
  */
enum Genre:
  case Biography
  case Children
  case Fantasy
  case History
  case Poetry
  case Romance
  case Thriller

object Genre:
  val All: List[Genre] = Genre.values.toList

  given Order[Genre] = Order.by(_.ordinal)
