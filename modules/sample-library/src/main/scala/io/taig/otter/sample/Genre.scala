package io.taig.otter.sample

import cats.Order
import io.taig.enumeration.ext.Mapping

/** What a book is, as a closed set.
  *
  * A Scala 3 `enum` and an `enumeration` schema over it, which is the pairing that keeps the wire spelling and the type
  * from drifting: [[Genre.mapping]] is checked for exhaustiveness at compile time, so adding a case here cannot
  * silently make it fail to read.
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
  given Order[Genre] = Order.by(_.ordinal)

  val mapping: Mapping[Genre, String] = Mapping.enumeration:
    case Genre.Biography => "biography"
    case Genre.Children  => "children"
    case Genre.Fantasy   => "fantasy"
    case Genre.History   => "history"
    case Genre.Poetry    => "poetry"
    case Genre.Romance   => "romance"
    case Genre.Thriller  => "thriller"
