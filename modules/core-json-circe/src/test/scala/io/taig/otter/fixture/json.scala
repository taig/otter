package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.Void
import io.taig.otter.component.JsonComponent.*

object json:
  val book: Json.Record[Book, Book] =
    (field("title", string) :* field("pages", int) :* field("read", boolean)).to[Book]

  val circle: Json.Record[Shape.Circle, Shape.Circle] = field("radius", double).toRecord.to[Shape.Circle]

  val square: Json.Record[Shape.Square, Shape.Square] = field("side", double).toRecord.to[Shape.Square]

  val shape: Json.Union[Shape, Shape] = (branch("circle", circle) :+ branch("square", square)).to[Shape]

  lazy val tree: Json.Record[Tree, Tree] =
    (field("value", int) :* field("children", collection.list(tree))).to[Tree]

  /** Can be written but not read: there is no way back from a title to a book. */
  val title: Json.Primitive.Text[Book, Void] = printer("title", _.title)

  /** Can be read but not written. */
  val isbn: Json.Primitive.Text[Void, Isbn] = parser("isbn", value => Right(Isbn(value)))
