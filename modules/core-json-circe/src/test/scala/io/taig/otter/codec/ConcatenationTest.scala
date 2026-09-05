package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.Census
import io.taig.otter.fixture.Tree
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** The four ways of writing one schema down. `:*` carries what it has built on the left and `*:` on the right, and the
  * empty root is a place to start rather than something to name, so all four denote the same nodes in the same order
  * and the same flat value shape.
  */
object ConcatenationTest extends ZIOSpecDefault:
  private val document: CirceJson = CirceJson.arr("Dune".asJson, 412.asJson, true.asJson)

  private val value: (String, Int, Boolean) = ("Dune", 412, true)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ConcatenationTest")(
    test("a tuple reads and writes the same however it is spelled"):
      val rooted = TNil :* string :* int :* boolean
      val appended = string :* int :* boolean
      val prepended = string *: int *: boolean *: TNil
      val bare = string *: int *: boolean

      assertTrue(
        JsonCirceEncoder.encode(rooted, value) == document,
        JsonCirceEncoder.encode(appended, value) == document,
        JsonCirceEncoder.encode(prepended, value) == document,
        JsonCirceEncoder.encode(bare, value) == document,
        JsonCirceDecoder.decode(rooted, document) == Validated.valid(value),
        JsonCirceDecoder.decode(appended, document) == Validated.valid(value),
        JsonCirceDecoder.decode(prepended, document) == Validated.valid(value),
        JsonCirceDecoder.decode(bare, document) == Validated.valid(value)
      )
    ,
    /** The shape is what the flattening is for, and a three member chain is where a pair would show. */
    test("all four flatten"):
      assertTrue(
        typeChecks("""val schema: Json.Tuple[(String, Int, Boolean)] = TNil :* string :* int :* boolean"""),
        typeChecks("""val schema: Json.Tuple[(String, Int, Boolean)] = string :* int :* boolean"""),
        typeChecks("""val schema: Json.Tuple[(String, Int, Boolean)] = string *: int *: boolean *: TNil"""),
        typeChecks("""val schema: Json.Tuple[(String, Int, Boolean)] = string *: int *: boolean""")
      )
    ,
    /** What the guard on the root less instance is there for: a receiver that already is a tuple keeps appending into
      * itself, rather than being wrapped in a fresh one.
      */
    test("a tuple already built is appended into rather than nested"):
      assertTrue(
        !typeChecks("""val schema: Json.Tuple[((String, Int), Boolean)] = string :* int :* boolean"""),
        !typeChecks("""val schema: Json.Tuple[(String, (Int, Boolean))] = string *: int *: boolean""")
      )
    ,
    test("the two operators mix"):
      val left = (string *: int) :* boolean
      val right = string *: (int :* boolean)

      assertTrue(
        JsonCirceEncoder.encode(left, value) == document,
        JsonCirceEncoder.encode(right, value) == document,
        typeChecks("""val schema: Json.Tuple[(String, Int, Boolean)] = (string *: int) :* boolean"""),
        typeChecks("""val schema: Json.Tuple[(String, Int, Boolean)] = string *: (int :* boolean)""")
      )
    ,
    /** A record was never rooted to begin with, so `RNil` is the spelling that gains a direction here. */
    test("a record reads and writes the same however it is spelled"):
      val object_ = CirceJson.obj("title" := "Dune", "pages" := 412, "read" := true)
      val book = Book("Dune", 412, true)
      val appended = (field("title", string) :* field("pages", int) :* field("read", boolean)).to[Book]
      val prepended = (field("title", string) *: field("pages", int) *: field("read", boolean) *: RNil).to[Book]
      val bare = (field("title", string) *: field("pages", int) *: field("read", boolean)).to[Book]

      assertTrue(
        JsonCirceEncoder.encode(appended, book) == object_,
        JsonCirceEncoder.encode(prepended, book) == object_,
        JsonCirceEncoder.encode(bare, book) == object_,
        JsonCirceDecoder.decode(prepended, object_) == Validated.valid(book),
        JsonCirceDecoder.decode(bare, object_) == Validated.valid(book)
      )
    ,
    /** The two directions are classified separately, which a member that can only be written is what tests: its read
      * side stays at `Any` and must not decide the shape of the side the schema does have.
      */
    test("a write only member leaves the members after it in place"):
      val schema: Json.Record.Writer[(Book, Int, Boolean)] =
        field("label", json.title) *: field("pages", int) *: field("read", boolean) *: RNil

      assertTrue(
        JsonCirceEncoder.encode(schema, (Book("Dune", 412, true), 412, true)) ==
          CirceJson.obj("label" := "Dune", "pages" := 412, "read" := true)
      )
    ,
    /** The operator is Scala's own on anything that is not a schema, which is what keeps it from being a nuisance to
      * import.
      */
    test("Scala's own cons is undisturbed"):
      assertTrue(typeChecks("""val tuple: (Int, String) = 1 *: "a" *: EmptyTuple"""))
    ,
    /** [[io.taig.otter.Prepend]] reduces once per member and nests to the right where [[io.taig.otter.Append]] nests to
      * the left, so a record wide enough to feel that is worth compiling. The twin of `json.census`.
      */
    test("a wide record conses"):
      assertTrue(typeChecks("""val schema: Json.Record[Census] =
        (field("first", string) *:
          field("second", string) *:
          field("third", string) *:
          field("fourth", string) *:
          field("fifth", string) *:
          field("sixth", string) *:
          field("seventh", string) *:
          field("eighth", string) *:
          field("ninth", string) *:
          field("tenth", string) *:
          field("eleventh", string) *:
          field("twelfth", string) *:
          field("thirteenth", string) *:
          field("fourteenth", string) *:
          field("fifteenth", string)).to"""))
    ,
    /** The left operand of `*:` is strict, so this is the shape that says recursion is not what it gives up: the schema
      * refers to itself from inside a child position, which is suspended where it is built.
      */
    test("a schema that refers to itself still prepends"):
      lazy val tree: Json.Record[Tree] = (
        field("value", int) *:
          field("children", collection.list(tree)) *:
          RNil
      ).to

      assertTrue(
        JsonCirceEncoder.encode(tree, Tree(1, List(Tree(2, Nil)))) == CirceJson.obj(
          "value" := 1,
          "children" := CirceJson.arr(CirceJson.obj("value" := 2, "children" := CirceJson.arr()))
        )
      )
  )
