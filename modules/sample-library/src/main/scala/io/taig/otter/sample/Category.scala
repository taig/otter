package io.taig.otter.sample

/** A shelf, and the shelves inside it.
  *
  * The schema for this refers to itself, which works only because it is named: a `$ref` needs something to point at,
  * and [[io.taig.otter.Keys.name]] is what puts a definition under `components/schemas` for it to point at. The Scala
  * side needs nothing special beyond a `lazy val` schema -- a `val` would capture the forward reference before it is
  * initialised.
  */
final case class Category(name: String, shelves: List[Category], holdings: Int)

object Category:
  /** A catalogue deep enough that a reader can see the recursion actually recurse. */
  val Root: Category = Category(
    name = "Everything",
    shelves = List(
      Category(
        name = "Fiction",
        shelves = List(
          Category("Fantasy", Nil, holdings = 2),
          Category("Thriller", Nil, holdings = 0)
        ),
        holdings = 2
      ),
      Category("Non-fiction", List(Category("History", Nil, holdings = 1)), holdings = 1)
    ),
    holdings = 3
  )
