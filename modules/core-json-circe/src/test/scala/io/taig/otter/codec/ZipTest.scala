package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.Isbn
import io.taig.otter.fixture.Shelf
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** `zip` is what `:*` does not mean for a whole record beside another. Both compile now that a schema beside a schema
  * is the tuple holding them, and they say different things: `:*` writes the two records as two elements of an array,
  * where `zip` writes both sets of fields into one object.
  */
object ZipTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ZipTest")(
    /** The decorating shape `zip` exists for: a record written in front of one that already has a name of its own. Both
      * sets of fields land in the same object, which is what tells this apart from a field holding a record.
      */
    test("a record zipped with a record writes both sets of fields side by side"):
      val schema: Json.Record[ZipTest.Detour] = field("distance", double).toRecord.zip(json.book).to
      val encoded = JsonCirceEncoder.encode(schema, ZipTest.Detour(1.5, Book("Dune", 412, true)))
      assertTrue(
        encoded == CirceJson.obj(
          "distance" -> CirceJson.fromDoubleOrNull(1.5),
          "title" -> CirceJson.fromString("Dune"),
          "pages" -> CirceJson.fromInt(412),
          "read" -> CirceJson.fromBoolean(true)
        ),
        JsonCirceDecoder.decode(schema, encoded) == Validated.valid(ZipTest.Detour(1.5, Book("Dune", 412, true)))
      )
    ,
    /** Where `:*` flattens, `zip` nests, so that the shape a call site sees does not turn on whether what is on the
      * left happens to be a tuple already.
      */
    test("the pair nests rather than flattening"):
      assertTrue(
        typeChecks("""val schema: Json.Record[((String, Int), Boolean)] = field("title", string).toRecord
          .zip(field("pages", int).toRecord)
          .zip(field("read", boolean).toRecord)"""),
        !typeChecks("""val schema: Json.Record[(String, Int, Boolean)] = field("title", string).toRecord
          .zip(field("pages", int).toRecord)
          .zip(field("read", boolean).toRecord)""")
      )
    ,
    /** The same, ascribed one direction at a time, so that neither is taken on the other's word. */
    test("both directions nest"):
      assertTrue(
        typeChecks("""val schema: Json.Record.Writer[((Book, Int), Boolean)] = field("label", json.title).toRecord
          .zip(field("pages", int).toRecord)
          .zip(field("read", boolean).toRecord)"""),
        !typeChecks("""val schema: Json.Record.Writer[(Book, Int, Boolean)] = field("label", json.title).toRecord
          .zip(field("pages", int).toRecord)
          .zip(field("read", boolean).toRecord)"""),
        typeChecks("""val schema: Json.Record.Reader[((Isbn, Int), Boolean)] = field("isbn", json.isbn).toRecord
          .zip(field("pages", int).toRecord)
          .zip(field("read", boolean).toRecord)"""),
        !typeChecks("""val schema: Json.Record.Reader[(Isbn, Int, Boolean)] = field("isbn", json.isbn).toRecord
          .zip(field("pages", int).toRecord)
          .zip(field("read", boolean).toRecord)""")
      )
    ,
    /** No match type is involved, unlike `:*`, so a direction a schema does not have cannot get the shape stuck: the
      * read side is simply `(Any, Book)`, which `Any` absorbs.
      */
    test("a write only side still zips"):
      val schema: Json.Record.Writer[(Isbn, Book)] = field("label", json.label).toRecord.zip(json.book)
      assertTrue(
        JsonCirceEncoder.encode(schema, (Isbn("978"), Book("Dune", 412, true))) == CirceJson.obj(
          "label" -> CirceJson.fromString("978"),
          "title" -> CirceJson.fromString("Dune"),
          "pages" -> CirceJson.fromInt(412),
          "read" -> CirceJson.fromBoolean(true)
        )
      )
    ,
    /** The same the other way around: the write side is `(Nothing, Book)`, which `Nothing` conforms to. */
    test("a read only side still zips"):
      val schema: Json.Record.Reader[(Isbn, Book)] = field("isbn", json.isbn).toRecord.zip(json.book)
      val document = CirceJson.obj(
        "isbn" -> CirceJson.fromString("978"),
        "title" -> CirceJson.fromString("Dune"),
        "pages" -> CirceJson.fromInt(412),
        "read" -> CirceJson.fromBoolean(true)
      )
      assertTrue(
        JsonCirceDecoder.decode(schema, document) == Validated.valid((Isbn("978"), Book("Dune", 412, true)))
      )
    ,
    /** Nothing has to be spelled out for `F` to be found, which is the whole point of reaching `Zip` through syntax
      * rather than by summoning the instance and writing the type lambda by hand.
      */
    test("two writers zip without a type argument"):
      val schema = json.shelf.zip(json.shelf)
      val shelf = Shelf(Isbn("978"), 412, true)
      assertTrue(
        JsonCirceEncoder
          .encode(schema, (shelf, shelf))
          .asObject
          .map(_.keys.toList)
          .contains(List("label", "pages", "read"))
      )
    ,
    /** A node carries the type of what is inside it, and zipping two records has to keep saying so: the result holds
      * what both sides held, which for two flat records is still nothing but primitives.
      */
    test("what the two sides hold widens to what fits both"):
      assertTrue(
        typeChecks("""val schema: Json.Record.Of[Json.Primitive.Node, (String, Int)] =
          field("title", string).toRecord.zip(field("pages", int).toRecord)"""),
        typeChecks("""val schema: Json.Record.Of[Json.Primitive.Node, (Book, Book)] =
          json.flatBook.zip(json.flatBook)"""),
        !typeChecks("""val schema: Json.Record.Of[Json.Primitive.Node, (Book, Book)] =
          json.flatBook.zip(json.book)""")
      )
    ,
    /** The receiver being the narrower of the two is the case a signature taking a single `F` from the prefix would
      * reject, and it is the one the decorating shape actually produces: a field's `S` is its schema's, while a record
      * given a name of its own is ascribed to hold anything.
      */
    test("a narrow receiver zips with a wider argument"):
      assertTrue(typeChecks("""val schema: Json.Record[(Double, Book)] =
        field("distance", double).toRecord.zip(json.book)"""))
    ,
    /** `Zip` is not the record's alone, so neither is the operator. */
    test("a tuple zips as a record does"):
      assertTrue(typeChecks("""val schema: Json.Tuple[(String, Int)] = string.toTuple.zip(int.toTuple)"""))
    ,
    /** The two records the class comment tells apart, side by side: one document each way. */
    test("appending two records is not zipping them"):
      val zipped: Json.Record[(Book, Book)] = json.book.zip(json.book)
      val appended: Json.Tuple[(Book, Book)] = json.book :* json.book
      val book = Book("Dune", 412, true)
      val object_ = CirceJson.obj("title" := "Dune", "pages" := 412, "read" := true)

      assertTrue(
        JsonCirceEncoder.encode(zipped, (book, book)) == object_,
        JsonCirceEncoder.encode(appended, (book, book)) == CirceJson.arr(object_, object_)
      )
  )

  final private case class Detour(distance: Double, book: Book)
