package io.taig.otter.fixture

import zio.test.Gen

import java.util.UUID
import scala.collection.immutable.SortedMap

/** Values for the schemas in [[json]], for the suites that state a property rather than an answer.
  *
  * Beside the schemas rather than beside an interpreter, for the same reason the schemas are: a value a `Json[Book]`
  * round trips is a claim about the schema, and every interpreter of it is held to the same one.
  */
object gen:
  val book: Gen[Any, Book] =
    for
      title <- Gen.alphaNumericString
      pages <- Gen.int
      read <- Gen.boolean
    yield Book(title, pages, read)

  val note: Gen[Any, Note] =
    for
      title <- Gen.alphaNumericString
      tag <- Gen.option(Gen.int)
    yield Note(title, tag)

  val shape: Gen[Any, Shape] =
    Gen.oneOf(
      Gen.double.map(Shape.Circle.apply),
      Gen.double.map(Shape.Square.apply),
      Gen.double.zip(Gen.double).map(Shape.Triangle.apply)
    )

  val verdict: Gen[Any, Verdict] =
    Gen.oneOf(
      Gen.const(Verdict.Accepted),
      Gen.const(Verdict.Rejected),
      Gen.alphaNumericString.map(Verdict.Deferred.apply)
    )

  def tree(depth: Int): Gen[Any, Tree] =
    if depth <= 0 then Gen.int.map(Tree(_, Nil))
    else
      for
        value <- Gen.int
        children <- Gen.listOfBounded(0, 2)(tree(depth - 1))
      yield Tree(value, children)

  val editions: Gen[Any, SortedMap[UUID, Int]] =
    Gen.listOf(Gen.uuid.zip(Gen.int)).map(SortedMap.from)

  val printings: Gen[Any, List[(Int, String)]] = Gen.listOf(Gen.int.zip(Gen.alphaNumericString))

  val genre: Gen[Any, Genre] = Gen.fromIterable(Genre.values.toList)

  val census: Gen[Any, Census] = Gen.alphaNumericString.map: value =>
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
