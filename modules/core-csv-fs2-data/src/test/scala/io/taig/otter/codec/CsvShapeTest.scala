package io.taig.otter.codec

import io.taig.otter.Csv
import io.taig.otter.component.CsvComponent.*
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.csv
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** CSV is flat, and a node carrying the type of what is inside it is what makes that a compile error rather than a
  * failure at conversion time. These are the negatives the format is defined by: what a cell may not hold, and what the
  * alphabet does not name at all.
  */
object CsvShapeTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvShapeTest")(
    test("a row of cells is what a schema is"):
      assertTrue(typeChecks("""val schema: Csv[Book] =
        (field("title", string) :* field("pages", int) :* field("read", boolean)).to[Book]"""))
    ,
    test("a row is flat, so it is also a row of primitives"):
      assertTrue(typeChecks("""val schema: Csv.Record.Of[Csv.Primitive.Node, Book] = csv.flatBook"""))
    ,
    /** A row is a row however it is spelled: the empty root is a place to start rather than something to name, and `*:`
      * is the same chain read the other way round.
      */
    test("the empty root is optional, in either direction"):
      assertTrue(
        typeChecks("""val schema: Csv[Book] =
          (field("title", string) *: field("pages", int) *: field("read", boolean) *: RNil).to[Book]"""),
        typeChecks("""val schema: Csv.Tuple[(String, Int, Boolean)] = string :* int :* boolean"""),
        typeChecks("""val schema: Csv.Tuple[(String, Int, Boolean)] = string *: int *: boolean *: TNil"""),
        !typeChecks("""val schema: Csv.Tuple[((String, Int), Boolean)] = string :* int :* boolean""")
      )
    ,
    /** A row is still not a cell, so neither spelling can put one in a column. */
    test("a column cannot hold another row"):
      assertTrue(!typeChecks("""field("book", csv.book) :* field("pages", int)"""))
    ,
    test("a cell cannot hold a row either"):
      assertTrue(
        !typeChecks("""field("book", csv.positional)"""),
        !typeChecks("""string :* csv.positional"""),
        !typeChecks("""csv.positional *: string""")
      )
    ,
    test("a row is not something a cell can hold, so it has no .optional"):
      assertTrue(
        !typeChecks("""csv.book.optional"""),
        !typeChecks("""csv.positional.optional"""),
        typeChecks("""int.optional""")
      )
    ,
    test("a row cannot be lifted into a positional one"):
      assertTrue(!typeChecks("""csv.book.toTuple"""), typeChecks("""int.toTuple"""))
    ,
    test("the alphabet does not name a union, so there is nothing to alternate"):
      assertTrue(!typeChecks("""field("a", int) :+ field("b", int)"""))
    ,
    test("a one directional schema can also say what it holds"):
      assertTrue(
        typeChecks("""val w: Csv.Writer.Of[Csv.Primitive.Text.Node, Book] = csv.title"""),
        !typeChecks("""val r: Csv.Reader.Of[Csv.Primitive.Text.Node, Book] = csv.title""")
      )
  )
