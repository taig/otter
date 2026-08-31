package io.taig.otter.fixture

import cats.Eq

final case class Book(title: String, pages: Int, read: Boolean)

/** Exercises an optional column, which can be written either way round. */
final case class Note(title: String, tag: Option[Int])

final case class Isbn(value: String)

object Isbn:
  given Eq[Isbn] = Eq.fromUniversalEquals

/** Exercises enumerations: a closed set of values written as text, which is all a cell has. */
enum Genre:
  case Fiction, History, Poetry
