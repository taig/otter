package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Csv
import io.taig.otter.component.CsvComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** A header is the one thing a CSV file says that no value takes part in, so it is a [[Renderer]] rather than an
  * encoder. What it has to guarantee is that its order is the order the encoder writes cells in.
  */
object CsvHeaderRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CsvHeaderRendererTest")(
    test("names the columns in order"):
      assertTrue(CsvHeaderRenderer.render(csv.book) == Chain("title", "pages", "read"))
    ,
    test("RNil has no columns"):
      assertTrue(CsvHeaderRenderer.render(RNil) == Chain.empty)
    ,
    test("a column is named through .optional and .optional(default)"):
      val schema = field("title", string) :* field("tag", int).optional :* field("pages", int).optional(0)
      assertTrue(CsvHeaderRenderer.render(schema) == Chain("title", "tag", "pages"))
    ,
    test("an attribute does not disturb the naming"):
      assertTrue(CsvHeaderRenderer.render(csv.omittedTag) == Chain("title", "tag"))
    ,
    test("the header lines up with the row the encoder writes"):
      val header = CsvHeaderRenderer.render(csv.blankTag)
      val absent = CsvRecordEncoder.encode(csv.blankTag, Note("Dune", none))
      val present = CsvRecordEncoder.encode(csv.blankTag, Note("Dune", 42.some))

      assertTrue(absent.map(_._1) == header, present.map(_._1) == header)
    ,
    test("a column may be named by a value rather than by a literal"):
      val column: Csv.Primitive.Text.Writer[Genre] = printer("column", _.toString.toLowerCase)
      val schema = field(Genre.Fiction, column, string) :* field(Genre.History, column, int)
      assertTrue(CsvHeaderRenderer.render(schema) == Chain("fiction", "history"))
    ,
    test("a schema that reads only still has a header, because no value takes part"):
      val schema = field("isbn", csv.isbn).toRecord
      assertTrue(CsvHeaderRenderer.render(schema) == Chain("isbn"))
  )
