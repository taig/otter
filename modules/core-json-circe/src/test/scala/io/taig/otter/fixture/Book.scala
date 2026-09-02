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

/** Exercises a name spelled as a value rather than as a literal. */
enum Tag:
  case Circle, Square, Triangle

/** Exercises unions and, through them, nested records. */
enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)
  case Triangle(base: Double, height: Double)

/** Exercises `Reference` laziness: the schema refers to itself. */
final case class Tree(value: Int, children: List[Tree])

/** Exercises a record whose first member can only be written: appending anything after such a member used to take the
  * record apart against the shape of its read side, which a write only member leaves at `Any`.
  */
final case class Shelf(label: Isbn, pages: Int, read: Boolean)

/** Exercises a record wide enough that copying the schema per member would be felt. */
final case class Census(
    first: String,
    second: String,
    third: String,
    fourth: String,
    fifth: String,
    sixth: String,
    seventh: String,
    eighth: String,
    ninth: String,
    tenth: String,
    eleventh: String,
    twelfth: String,
    thirteenth: String,
    fourteenth: String,
    fifteenth: String
)
