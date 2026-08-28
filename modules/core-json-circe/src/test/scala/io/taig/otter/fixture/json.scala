package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*

object json:
  val book: Json.Record[Book] =
    (field("title", string) :* field("pages", int) :* field("read", boolean)).to[Book]

  val genre: Json.Enumeration[Genre] = enumeration(string):
    case Genre.Fiction => "fiction"
    case Genre.History => "history"
    case Genre.Poetry  => "poetry"

  val circle: Json.Record[Shape.Circle] = field("radius", double).toRecord.to[Shape.Circle]

  val square: Json.Record[Shape.Square] = field("side", double).toRecord.to[Shape.Square]

  val triangle: Json.Record[Shape.Triangle] =
    (field("base", double) :* field("height", double)).to[Shape.Triangle]

  /** Three branches, so the union nests two levels deep. */
  val shape: Json.Union[Shape] =
    (branch("circle", circle) :+ branch("square", square) :+ branch("triangle", triangle)).to[Shape]

  lazy val tree: Json.Record[Tree] =
    (field("value", int) :* field("children", collection.list(tree))).to[Tree]

  /** The same three fields, ascribed to say that every one of them is a primitive. That is what a flat format can
    * represent, and it is a compile error to write this down for a schema that nests.
    */
  val flatBook: Json.Record.Of[Json.Primitive.Node, Book] =
    (field("title", string) :* field("pages", int) :* field("read", boolean)).to[Book]

  /** Can be written but not read: there is no way back from a title to a book. */
  val title: Json.Primitive.Text.Writer[Book] = printer("title", _.title)

  /** Can be read but not written. */
  val isbn: Json.Primitive.Text.Reader[Isbn] = parser("isbn", value => Right(Isbn(value)))
