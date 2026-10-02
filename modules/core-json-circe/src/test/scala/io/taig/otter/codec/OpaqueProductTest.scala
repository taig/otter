package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.OpaqueProducts
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

object OpaqueProductTest extends ZIOSpecDefault:
  val isbn: Json.Primitive.Text[OpaqueProducts.Isbn] =
    codec("isbn", value => Right(OpaqueProducts.Isbn(value)), OpaqueProducts.Isbn.render)

  private val value = OpaqueProducts.Book(OpaqueProducts.Isbn("9780261102217"), "The Hobbit", 1)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpaqueProductTest")(
    test("records convert opaque members in both operator directions"):
      val appended = (field("isbn", isbn) :* field("title", string) :* field("edition", int)).to[OpaqueProducts.Book]
      val prepended =
        (field("isbn", isbn) *: field("title", string) *: field("edition", int) *: RNil).to[OpaqueProducts.Book]
      val document = CirceJson.obj("isbn" := "9780261102217", "title" := "The Hobbit", "edition" := 1)

      assertTrue(
        JsonCirceEncoder.encode(appended, value) == document,
        JsonCirceEncoder.encode(prepended, value) == document,
        JsonCirceDecoder.decode(appended, document) == Validated.valid(value),
        JsonCirceDecoder.decode(prepended, document) == Validated.valid(value)
      )
    ,
    test("tuples convert opaque members and drop constants without reordering"):
      val appended =
        (TNil :* constant(string, "book") :* isbn :* constant(int, 0) :* string :* int).to[OpaqueProducts.Book]
      val prepended =
        (constant(string, "book") *: isbn *: constant(int, 0) *: string *: int *: TNil).to[OpaqueProducts.Book]
      val document = CirceJson.arr("book".asJson, "9780261102217".asJson, 0.asJson, "The Hobbit".asJson, 1.asJson)

      assertTrue(
        JsonCirceEncoder.encode(appended, value) == document,
        JsonCirceEncoder.encode(prepended, value) == document,
        JsonCirceDecoder.decode(appended, document) == Validated.valid(value),
        JsonCirceDecoder.decode(prepended, document) == Validated.valid(value)
      )
    ,
    test("an opaque member works at either end and in the middle"):
      assertTrue(
        typeChecks("""val schema: Json.Tuple[(String, OpaqueProducts.Isbn, Int)] = string :* isbn :* int"""),
        typeChecks("""val schema: Json.Tuple[(String, OpaqueProducts.Isbn, Int)] = string *: isbn *: int"""),
        typeChecks("""val schema: Json.Tuple[(String, Int, OpaqueProducts.Isbn)] = string :* int :* isbn"""),
        typeChecks("""val schema: Json.Tuple[(String, Int, OpaqueProducts.Isbn)] = string *: int *: isbn""")
      )
    ,
    test("a single opaque member converts with Unit on either side"):
      assertTrue(
        typeChecks("""val schema: Json.Record[OpaqueProducts.Identifier] = (RNil :* field("isbn", isbn)).to"""),
        typeChecks("""val schema: Json.Record[OpaqueProducts.Identifier] = (field("isbn", isbn) *: RNil).to"""),
        typeChecks("""val schema: Json.Tuple[OpaqueProducts.Identifier] = (isbn :* constant(int, 0)).to"""),
        typeChecks("""val schema: Json.Tuple[OpaqueProducts.Identifier] = (constant(int, 0) *: isbn).to""")
      )
    ,
    test("hidden tuples stay atomic and hidden Unit members are retained"):
      val pair: Json.Primitive.Text[OpaqueProducts.Pair] =
        codec("pair", value => Right(OpaqueProducts.Pair(value, 1)), OpaqueProducts.Pair.label)
      val marker: Json.Primitive.Text[OpaqueProducts.Marker] =
        codec("marker", _ => Right(OpaqueProducts.Marker()), _ => "present")
      val appended = (pair :* marker :* isbn).to[OpaqueProducts.Wrapped]
      val prepended = (pair *: marker *: isbn).to[OpaqueProducts.Wrapped]
      val wrapped = OpaqueProducts.Wrapped(OpaqueProducts.Pair("book", 1), OpaqueProducts.Marker(), value.isbn)
      val document = CirceJson.arr("book".asJson, "present".asJson, "9780261102217".asJson)
      val right: Json.Tuple[(OpaqueProducts.Isbn, OpaqueProducts.Pair)] = isbn *: pair
      val rightDocument = CirceJson.arr("9780261102217".asJson, "book".asJson)

      assertTrue(
        JsonCirceEncoder.encode(appended, wrapped) == document,
        JsonCirceEncoder.encode(prepended, wrapped) == document,
        JsonCirceDecoder.decode(appended, document) == Validated.valid(wrapped),
        JsonCirceDecoder.decode(prepended, document) == Validated.valid(wrapped),
        JsonCirceEncoder.encode(right, (value.isbn, wrapped.pair)) == rightDocument,
        JsonCirceDecoder.decode(right, rightDocument) == Validated.valid((value.isbn, wrapped.pair))
      )
    ,
    test("conversion still rejects the wrong product order"):
      assertTrue(
        !typeChecks(
          """(field("isbn", isbn) :* field("title", string) :* field("edition", int)).to[OpaqueProducts.Reordered]"""
        ),
        !typeChecks("""(isbn *: string *: int).to[OpaqueProducts.Reordered]""")
      )
    ,
    test("the representation remains inaccessible outside the domain API"):
      assertTrue(
        !typeChecks("""val value: OpaqueProducts.Isbn = "9780261102217""""),
        !typeChecks("""val value: String = OpaqueProducts.Isbn("9780261102217")"""),
        !typeChecks("""OpaqueProducts.Isbn("9780261102217").length""")
      )
  )
