package io.taig.openapi.validation

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.taig.openapi.validation.validations.*
import munit.FunSuite

final class ValidationsTest extends FunSuite:
  test("collection.atLeast") {
    assert(collection.list.atLeast(reference = 1).run(List(1, 2, 3)).isValid)
    assert(collection.list.atLeast(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = collection.list.atLeast(reference = 3).run(List(1)),
      expected = Constraint.collection.atLeast(reference = 3).toViolation(actual = 1L).invalidNec
    )
  }

  test("collection.atMost") {
    assert(collection.list.atMost(reference = 3).run(List(1)).isValid)
    assert(collection.list.atMost(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = collection.list.atMost(reference = 1).run(List(1, 2, 3)),
      expected = Constraint.collection.atMost(reference = 1).toViolation(actual = 3L).invalidNec
    )
  }

  test("collection.empty") {
    assert(collection.list.empty.run(Nil).isValid)
    assertEquals(
      obtained = collection.list.empty.run(List(1, 2, 3)),
      expected = Constraint.collection.empty.toViolation(actual = List(1, 2, 3)).invalidNec
    )
  }

  test("collection.nonEmpty") {
    assert(collection.list.nonEmpty.run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = collection.list.nonEmpty.run(Nil),
      expected = Constraint.collection.nonEmpty.toViolation(actual = Nil).invalidNec
    )
  }

  test("collection.exactly") {
    assert(collection.list.exactly(reference = 3).run(List(1, 2, 3)).isValid)
    assert(collection.list.exactly(reference = 0).run(Nil).isValid)
    assertEquals(
      obtained = collection.list.exactly(reference = 3).run(List(1)),
      expected = Constraint.collection.exactly(reference = 3L).toViolation(actual = 1L).invalidNec
    )
  }

  test("collection.contains") {
    assert(collection.list.contains(reference = "foobar").run(List("foo", "foobar", "bar")).isValid)
    assertEquals(
      obtained = collection.list.contains(reference = "foobar").run(List("foo", "bar")),
      expected =
        Constraint.collection.contains(reference = "foobar").toViolation(actual = List("foo", "bar")).invalidNec
    )
  }

//  test("time.after: Instant") {
//    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)
//
//    assert(time.after(sample).run(sample).isValid)
//    assert(time.after(sample).run(sample.plusSeconds(100)).isValid)
//    assertEquals(
//      obtained = time.after(sample).run(sample.minusSeconds(100)).error,
//      expected = Violation(
//        Constraint.time.after(sample.atZone(ZoneOffset.UTC)),
//        actual = sample.minusSeconds(100)
//      ).some
//    )
//  }
//
//  test("time.before: Instant") {
//    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)
//
//    assert(time.before(sample).run(sample).isValid)
//    assert(time.before(sample).run(sample.minusSeconds(100)).isValid)
//    assertEquals(
//      obtained = time.before(sample).run(sample.plusSeconds(100)).error,
//      expected = Violation(
//        Constraint.time.before(sample.atZone(ZoneOffset.UTC)),
//        actual = sample.plusSeconds(100)
//      ).some
//    )
//  }

  test("numeric.greaterThan") {
    val comparison = NumericComparison(reference = 1d, equal = true, delta = 0.5d.some)

    assertEquals(
      obtained = numeric.greaterThan(comparison).run(0.75d),
      expected = ().valid
    )
    assertEquals(
      obtained = numeric.greaterThan(comparison).run(0.5d),
      expected = ().valid
    )
    assertEquals(
      obtained = numeric.greaterThan(comparison.withEqual(false)).run(0.5d),
      expected = Constraint.numeric.greaterThan(comparison.withEqual(false)).toViolation(actual = 0.5d).invalidNec
    )
    assertEquals(
      obtained = numeric.greaterThan(comparison).run(0.25d),
      expected = Constraint.numeric.greaterThan(comparison).toViolation(actual = 0.25d).invalidNec
    )
  }

  test("numeric.lessThan") {
    val comparison = NumericComparison(reference = 1d, equal = true, delta = 0.5d.some)

    assertEquals(obtained = numeric.lessThan(comparison).run(1.25d), expected = ().valid)
    assertEquals(obtained = numeric.lessThan(comparison).run(1.5d), expected = ().valid)
    assertEquals(
      obtained = numeric.lessThan(comparison.withEqual(false)).run(1.5d),
      expected = Constraint.numeric.lessThan(comparison.withEqual(false)).toViolation(actual = 1.5d).invalidNec
    )
    assertEquals(
      obtained = numeric.lessThan(comparison).run(1.75d),
      expected = Constraint.numeric.lessThan(comparison).toViolation(actual = 1.75d).invalidNec
    )
  }

  test("numeric.equal") {
    assert(numeric.equal(3).run(3).isValid)
    assert(numeric.equal(3f).run(3f).isValid)
    assert(numeric.equal(3d).run(3d).isValid)

    assertEquals(
      obtained = numeric.equal(1).run(3),
      expected = Violation(Constraint.numeric.equal(NumericComparison.equal(1)), actual = 3).invalidNec
    )
    assertEquals(
      obtained = numeric.equal(0.3d).run(0.1d + 0.2d),
      expected = Violation(Constraint.numeric.equal(NumericComparison.equal(0.3d)), actual = 0.1d + 0.2d).invalidNec
    )
  }

  test("numeric.equal (delta)") {
    assert(numeric.equal(reference = 0.3d, delta = 0.01d.some).run(0.1d + 0.2d).isValid)
    assert(numeric.equal(reference = 0.3d, delta = 0.01d.some).run(0.1d + 0.25d).isInvalid)
    assert(numeric.equal(reference = 0.3d, delta = 0.01d.some).run(0.1d + 0.15d).isInvalid)
  }

  test("parse.uuid") {
    assert(parser.uuid.run("07e7793c-c09e-4556-9b80-8b6872705b8e").isValid)
    assertEquals(
      obtained = parser.uuid.run("foobar"),
      expected = Constraint.parser("UUID").toViolation("foobar").invalidNec
    )
  }

  test("text.atLeast") {
    assert(text.atLeast(reference = 1).run("foo").isValid)
    assert(text.atLeast(reference = 3).run("foo").isValid)
    assertEquals(
      obtained = text.atLeast(reference = 3).run("fo"),
      expected = Constraint.text.atLeast(reference = 3).toViolation(actual = 2).invalidNec
    )
  }

  test("text.atMost") {
    assert(text.atMost(reference = 3).run("fo").isValid)
    assert(text.atMost(reference = 3).run("foo").isValid)
    assertEquals(
      obtained = text.atMost(reference = 1).run("foo"),
      expected = Constraint.text.atMost(reference = 1).toViolation(actual = 3).invalidNec
    )
  }

  test("text.empty") {
    assert(text.empty.run("").isValid)
    assertEquals(
      obtained = text.empty.run("foo"),
      expected = Constraint.text.atMost(reference = 0).toViolation(actual = 3).invalidNec
    )
  }

  test("text.length") {
    assertEquals(obtained = text.length.run(""), expected = 0.valid)
    assertEquals(obtained = text.length.run("foobar"), expected = 6.valid)
  }

  test("text.nonEmpty") {
    assert(text.nonEmpty.run("foobar").isValid)
    assert(text.nonEmpty.run(" ").isValid)
    assertEquals(
      obtained = text.nonEmpty.run(""),
      expected = Constraint.required.toViolation(actual = "").invalidNec
    )
  }

  test("text.required") {
    assertEquals(obtained = text.required.run("foobar"), expected = "foobar".valid)
    assertEquals(obtained = text.required.run(" foo  bar  "), expected = "foo  bar".valid)
    assertEquals(
      obtained = text.required.run(""),
      expected = Constraint.required.toViolation(actual = "").invalidNec
    )
  }

  test("text.trim") {
    assertEquals(obtained = text.trim.run(""), expected = "".valid)
    assertEquals(obtained = text.trim.run(" foo  bar   "), expected = "foo  bar".valid)
  }

  test("text.exactly") {
    assert(text.exactly(reference = 3).run("foo").isValid)
    assert(text.exactly(reference = 0).run("").isValid)
    assertEquals(
      obtained = text.exactly(reference = 1).run("foo"),
      expected = Constraint.text.exactly(reference = 1).toViolation(actual = 3).invalidNec
    )
  }

  test("text.matches") {
    val Whitespace = "\\s+".r

    assert(text.matches(regex = Whitespace).run("   ").isValid)
    assertEquals(
      obtained = text.matches(regex = Whitespace).run(" foobar "),
      expected = Constraint.text.matches(regex = Whitespace).toViolation(actual = " foobar ").invalidNec
    )
    assertEquals(
      obtained = text.matches(regex = Whitespace).run(""),
      expected = Constraint.text.matches(regex = Whitespace).toViolation(actual = "").invalidNec
    )
  }
