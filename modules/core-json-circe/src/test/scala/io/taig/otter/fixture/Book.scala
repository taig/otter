package io.taig.otter.fixture

import cats.Eq

final case class Book(title: String, pages: Int, read: Boolean)

/** Exercises an optional field, which can be written either way round. */
final case class Note(title: String, tag: Option[Int])

final case class Isbn(value: String)

object Isbn:
  given Eq[Isbn] = Eq.fromUniversalEquals

/** Exercises enumerations: a closed set of values written as text. */
enum Genre:
  case Fiction, History, Poetry

/** Exercises unions and, through them, nested records. */
enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)
  case Triangle(base: Double, height: Double)

/** Exercises `Reference` laziness: the schema refers to itself. */
final case class Tree(value: Int, children: List[Tree])
