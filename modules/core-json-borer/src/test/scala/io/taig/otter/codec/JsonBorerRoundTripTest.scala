package io.taig.otter.codec

import cats.data.Validated
import io.bullet.borer.Json as BorerJson
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

import java.util.UUID
import scala.collection.immutable.SortedMap

/** The one claim [[JsonBorerAgreementTest]] cannot make: that this module's encoder and decoder agree with *each
  * other*. A matched pair of bugs -- a write and a read that are wrong the same way -- agrees with circe on the way in
  * and is still wrong on the way out, and only a round trip catches it.
  *
  * The trip goes through bytes rather than through a document, which is the whole point of the module: the encoder
  * never builds one, so there is nothing in the middle to compare and the wire is the only meeting place.
  */
object JsonBorerRoundTripTest extends ZIOSpecDefault:
  private def roundTrips[A](schema: Json[A], value: A): TestResult =
    val encoded = BorerJson.encode(value)(using JsonBorer.encoder(schema)).toByteArray
    val decoded = BorerJson.decode(encoded).to(using JsonBorer.validated(schema)).value

    assertTrue(decoded == Validated.valid(value))

  private val book: Gen[Any, Book] =
    for
      title <- Gen.alphaNumericString
      pages <- Gen.int
      read <- Gen.boolean
    yield Book(title, pages, read)

  private val note: Gen[Any, Note] =
    for
      title <- Gen.alphaNumericString
      tag <- Gen.option(Gen.int)
    yield Note(title, tag)

  private val shape: Gen[Any, Shape] =
    Gen.oneOf(
      Gen.double.map(Shape.Circle.apply),
      Gen.double.map(Shape.Square.apply),
      Gen.double.zip(Gen.double).map(Shape.Triangle.apply)
    )

  private val verdict: Gen[Any, Verdict] =
    Gen.oneOf(
      Gen.const(Verdict.Accepted),
      Gen.const(Verdict.Rejected),
      Gen.alphaNumericString.map(Verdict.Deferred.apply)
    )

  private def tree(depth: Int): Gen[Any, Tree] =
    if depth <= 0 then Gen.int.map(Tree(_, Nil))
    else
      for
        value <- Gen.int
        children <- Gen.listOfBounded(0, 2)(tree(depth - 1))
      yield Tree(value, children)

  private val editions: Gen[Any, SortedMap[UUID, Int]] =
    Gen.listOf(Gen.uuid.zip(Gen.int)).map(SortedMap.from)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerRoundTripTest")(
    test("case class"):
      check(book)(roundTrips(json.book, _))
    ,
    test("enum through a union"):
      check(shape)(roundTrips(json.shape, _))
    ,
    test("enum through a union whose branches read the same type"):
      check(verdict)(roundTrips(json.verdict, _))
    ,
    test("optional field, omitted"):
      check(note)(roundTrips(json.omittedTag, _))
    ,
    test("optional field, nullable"):
      check(note)(roundTrips(json.nullableTag, _))
    ,
    test("two layers of absence, kept apart by a strict field"):
      check(Gen.option(Gen.option(Gen.int)))(roundTrips(json.nestedTag, _))
    ,
    test("enumeration"):
      check(Gen.fromIterable(Genre.values.toList))(roundTrips(json.genre, _))
    ,
    test("recursive schema"):
      check(tree(depth = 3))(roundTrips(json.tree, _))
    ,
    test("enum through a union whose branches are named by a value"):
      check(shape)(roundTrips(json.taggedShape, _))
    ,
    test("dictionary with a typed key"):
      check(editions)(roundTrips(json.editions, _))
    ,
    test("dictionary with an integer key, which the document holds as text"):
      check(Gen.listOf(Gen.int.zip(Gen.alphaNumericString)))(roundTrips(json.printings, _))
    ,
    test("a 15 field record"):
      check(Gen.alphaNumericString): value =>
        roundTrips(
          json.census,
          Census(
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value,
            value
          )
        )
  )
