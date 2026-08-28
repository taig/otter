package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Json
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

object JsonCirceRoundTripTest extends ZIOSpecDefault:
  private def roundTrips[A](schema: Json[A], value: A): TestResult =
    val encoded = JsonCirceEncoder.encode(schema, value)
    assertTrue(JsonCirceDecoder.decode(schema, encoded) == Validated.valid(value))

  private val book: Gen[Any, Book] =
    for
      title <- Gen.alphaNumericString
      pages <- Gen.int
      read <- Gen.boolean
    yield Book(title, pages, read)

  private val shape: Gen[Any, Shape] =
    Gen.oneOf(
      Gen.double.map(Shape.Circle.apply),
      Gen.double.map(Shape.Square.apply),
      Gen.double.zip(Gen.double).map(Shape.Triangle.apply)
    )

  private def tree(depth: Int): Gen[Any, Tree] =
    if depth <= 0 then Gen.int.map(Tree(_, Nil))
    else
      for
        value <- Gen.int
        children <- Gen.listOfBounded(0, 2)(tree(depth - 1))
      yield Tree(value, children)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonCirceRoundTripTest")(
    test("case class"):
      check(book)(roundTrips(json.book, _))
    ,
    test("enum through a union"):
      check(shape)(roundTrips(json.shape, _))
    ,
    test("enumeration"):
      check(Gen.fromIterable(Genre.values.toList))(roundTrips(json.genre, _))
    ,
    test("recursive schema"):
      check(tree(depth = 3))(roundTrips(json.tree, _))
  )
