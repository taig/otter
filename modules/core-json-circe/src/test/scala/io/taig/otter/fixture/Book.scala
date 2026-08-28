package io.taig.otter.fixture

import cats.Eq

final case class Book(title: String, pages: Int, read: Boolean)

final case class Isbn(value: String)

object Isbn:
  given Eq[Isbn] = Eq.fromUniversalEquals

/** Exercises unions and, through them, nested records. */
enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)

/** Exercises `Reference` laziness: the schema refers to itself. */
final case class Tree(value: Int, children: List[Tree])
