package io.taig.screening

import cats.syntax.all.*
import io.taig.screening.syntax.*
import io.taig.screening.validations.*
import munit.FunSuite

import java.time.*

final class ValidationsTest extends FunSuite:
//  test("collection.atLeast") {
//    assert(list.atLeast(reference = 1).run(List(1, 2, 3)).isValid)
//    assert(list.atLeast(reference = 3).run(List(1, 2, 3)).isValid)
//    assertEquals(
//      obtained = list.atLeast(reference = 3).run(List(1)).error,
//      expected = Violation(Constraint.numeric.greaterThan(reference = 3), actual = 1).some
//    )
//  }
//
//  test("collection.atMost") {
//    assert(list.atMost(reference = 3).run(List(1)).isValid)
//    assert(list.atMost(reference = 3).run(List(1, 2, 3)).isValid)
//    assertEquals(
//      obtained = list.atMost(reference = 1).run(List(1, 2, 3)).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 1), actual = 3).some
//    )
//  }
//
//  test("collection.empty") {
//    assert(list.empty.run(Nil).isValid)
//    assertEquals(
//      obtained = list.empty.run(List(1, 2, 3)).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 0), actual = 3).some
//    )
//  }
//
//  test("collection.nonEmpty") {
//    assert(list.nonEmpty.run(List(1, 2, 3)).isValid)
//    assertEquals(
//      obtained = list.nonEmpty.run(Nil).error,
//      expected = Violation(Constraint.numeric.greaterThan(reference = 1), actual = 0).some
//    )
//  }
//
//  test("collection.exactly") {
//    assert(list.exactly(reference = 3).run(List(1, 2, 3)).isValid)
//    assert(list.exactly(reference = 0).run(Nil).isValid)
//    assertEquals(
//      obtained = list.exactly(reference = 3).run(List(1)).error,
//      expected = Violation(Constraint.numeric.equal(reference = 3), actual = 1).some
//    )
//  }
//
//  test("collection.contains") {
//    assert(list.contains(reference = "foobar").run(List("foo", "foobar", "bar")).isValid)
//    assertEquals(
//      obtained = list.contains(reference = "foobar").run(List("foo", "bar")).error,
//      expected = Violation(Constraint.collection.contains(reference = "foobar"), actual = List("foo", "bar")).some
//    )
//  }
//
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
    assertEquals(
      obtained = numeric.greaterThan(reference = 1d, equal = true, delta = 0.5d.some).run(0.75d),
      expected = ().valid
    )
    assertEquals(
      obtained = numeric.greaterThan(reference = 1d, equal = true, delta = 0.5d.some).run(0.5d),
      expected = ().valid
    )
    assertEquals(
      obtained = numeric.greaterThan(reference = 1d, equal = false, delta = 0.5d.some).run(0.5d),
      expected = constraints.numeric
        .greaterThan(reference = 1d, equal = false, delta = 0.5d.some)
        .toViolation(actual = 0.5d)
        .invalidNec
    )
    assertEquals(
      obtained = numeric.greaterThan(reference = 1d, equal = true, delta = 0.5d.some).run(0.25d),
      expected = constraints.numeric
        .greaterThan(reference = 1d, equal = true, delta = 0.5d.some)
        .toViolation(actual = 0.25d)
        .invalidNec
    )
  }

//  test("numeric.greaterThanNotEqual") {
//    assert(numeric.greaterThanNotEqual(reference = 1).run(3).isValid)
//    assertEquals(
//      obtained = numeric.greaterThanNotEqual(reference = 3).run(3).error,
//      expected = Violation(Constraint.numeric.greaterThan(reference = 3, equal = false), actual = 3).some
//    )
//    assertEquals(
//      obtained = numeric.greaterThanNotEqual(3).run(1).error,
//      expected = Violation(Constraint.numeric.greaterThan(reference = 3, equal = false), actual = 1).some
//    )
//  }
//
//  test("numeric.greaterThanEqual") {
//    assert(numeric.greaterThanEqual(reference = 1).run(3).isValid)
//    assert(numeric.greaterThanEqual(reference = 3).run(3).isValid)
//    assertEquals(
//      obtained = numeric.greaterThanEqual(3).run(1).error,
//      expected = Violation(Constraint.numeric.greaterThan(reference = 3), actual = 1).some
//    )
//  }

  test("numeric.lessThan") {
    assertEquals(
      obtained = numeric.lessThan(reference = 1d, equal = true, delta = 0.5d.some).run(1.25d),
      expected = ().valid
    )
    assertEquals(
      obtained = numeric.lessThan(reference = 1d, equal = true, delta = 0.5d.some).run(1.5d),
      expected = ().valid
    )
    assertEquals(
      obtained = numeric.lessThan(reference = 1d, equal = false, delta = 0.5d.some).run(1.5d),
      expected = constraints.numeric
        .lessThan(reference = 1d, equal = false, delta = 0.5d.some)
        .toViolation(actual = 1.5d)
        .invalidNec
    )
    assertEquals(
      obtained = numeric.lessThan(reference = 1d, equal = true, delta = 0.5d.some).run(1.75d),
      expected = constraints.numeric
        .lessThan(reference = 1d, equal = true, delta = 0.5d.some)
        .toViolation(actual = 1.75d)
        .invalidNec
    )
  }

//
//  test("numeric.lessThanNotEqual") {
//    assert(numeric.lessThanNotEqual(reference = 3).run(1).isValid)
//    assertEquals(
//      obtained = numeric.lessThanNotEqual(reference = 3).run(3).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 3, equal = false), actual = 3).some
//    )
//    assertEquals(
//      obtained = numeric.lessThanNotEqual(1).run(3).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 1, equal = false), actual = 3).some
//    )
//  }
//
//  test("numeric.lessThan (delta)") {
//    assert(numeric.lessThanEqual(reference = 1d, delta = 0.5d).run(1.25d).isValid)
//    assert(numeric.lessThanEqual(reference = 1d, delta = 0.5d).run(1.5d).isValid)
//    assertEquals(
//      obtained = numeric.lessThanNotEqual(reference = 1d, delta = 0.5d).run(1.5d).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 1d, delta = 0.5d, equal = false), actual = 1.5).some
//    )
//    assertEquals(
//      obtained = numeric.lessThanEqual(reference = 1d, delta = 0.5d).run(1.75d).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 1d, delta = 0.5d), actual = 1.75).some
//    )
//  }
//
//  test("numeric.lessThanEqual") {
//    assert(numeric.lessThanEqual(reference = 3).run(1).isValid)
//    assert(numeric.lessThanEqual(reference = 3).run(3).isValid)
//    assertEquals(
//      obtained = numeric.lessThanEqual(1).run(3).error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 1), actual = 3).some
//    )
//  }
//
//  test("numeric.equal") {
//    assert(numeric.equal(3).run(3).isValid)
//    assert(numeric.equal(3f).run(3f).isValid)
//    assert(numeric.equal(3d).run(3d).isValid)
//
//    assertEquals(
//      obtained = numeric.equal(1).run(3).error,
//      expected = Violation(Constraint.numeric.equal(reference = 1), actual = 3).some
//    )
//    assertEquals(
//      obtained = numeric.equal(0.3d, 0d).run(0.1d + 0.2d).error,
//      expected = Violation(Constraint.numeric.equal(reference = 0.3d), actual = 0.1d + 0.2d).some
//    )
//  }
//
//  test("numeric.equal (delta)") {
//    assert(numeric.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.2d).isValid)
//    assert(numeric.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.25d).isInvalid)
//    assert(numeric.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.15d).isInvalid)
//  }
//
//  test("parsing.bigDecimal") {
//    assert(parsing.bigDecimal.run("3.14").isValid)
//    assert(parsing.bigDecimal.run("3").isValid)
//    assertEquals(
//      obtained = parsing.bigDecimal.run("foobar").error,
//      expected = Violation(Constraint.parsing("bigDecimal"), actual = "foobar").some
//    )
//  }
//
//  test("parsing.bigInt") {
//    assert(parsing.bigInt.run("3").isValid)
//    assert(parsing.bigInt.run("0").isValid)
//    assertEquals(
//      obtained = parsing.bigInt.run("3.14").error,
//      expected = Violation(Constraint.parsing("bigInt"), actual = "3.14").some
//    )
//    assertEquals(
//      obtained = parsing.bigInt.run("foobar").error,
//      expected = Violation(Constraint.parsing("bigInt"), actual = "foobar").some
//    )
//  }
//
//  test("parsing.double") {
//    assert(parsing.double.run("3.14").isValid)
//    assert(parsing.double.run("3").isValid)
//    assertEquals(
//      obtained = parsing.double.run("foobar").error,
//      expected = Violation(Constraint.parsing("double"), actual = "foobar").some
//    )
//  }
//
//  test("parsing.float") {
//    assert(parsing.float.run("3.14").isValid)
//    assert(parsing.float.run("3").isValid)
//    assertEquals(
//      obtained = parsing.float.run("foobar").error,
//      expected = Violation(Constraint.parsing("float"), actual = "foobar").some
//    )
//  }
//
//  test("parsing.int") {
//    assert(parsing.int.run("3").isValid)
//    assert(parsing.int.run("0").isValid)
//    assertEquals(
//      obtained = parsing.int.run("3.14").error,
//      expected = Violation(Constraint.parsing("int"), actual = "3.14").some
//    )
//    assertEquals(
//      obtained = parsing.int.run("foobar").error,
//      expected = Violation(Constraint.parsing("int"), actual = "foobar").some
//    )
//  }
//
//  test("parsing.long") {
//    assert(parsing.long.run("3").isValid)
//    assert(parsing.long.run("0").isValid)
//    assertEquals(
//      obtained = parsing.long.run("3.14").error,
//      expected = Violation(Constraint.parsing("long"), actual = "3.14").some
//    )
//    assertEquals(
//      obtained = parsing.long.run("foobar").error,
//      expected = Violation(Constraint.parsing("long"), actual = "foobar").some
//    )
//  }
//
//  test("parsing.short") {
//    assert(parsing.short.run("3").isValid)
//    assert(parsing.short.run("0").isValid)
//    assertEquals(
//      obtained = parsing.short.run("3.14").error,
//      expected = Violation(Constraint.parsing("short"), actual = "3.14").some
//    )
//    assertEquals(
//      obtained = parsing.short.run("foobar").error,
//      expected = Violation(Constraint.parsing("short"), actual = "foobar").some
//    )
//  }

  test("text.atLeast") {
    assert(text.atLeast(reference = 1).run("foo").isValid)
    assert(text.atLeast(reference = 3).run("foo").isValid)
    assertEquals(
      obtained = text.atLeast(reference = 3).run("fo"),
      expected = constraints.text.atLeast(reference = 3).toViolation(actual = "fo").invalidNec
    )
  }

//  test("text.atMost") {
//    assert(text.atMost(reference = 3).run("fo").isValid)
//    assert(text.atMost(reference = 3).run("foo").isValid)
//    assertEquals(
//      obtained = text.atMost(reference = 1).run("foo").error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 1), actual = 3).some
//    )
//  }
//
//  test("text.empty") {
//    assert(text.empty.run("").isValid)
//    assertEquals(
//      obtained = text.empty.run("foo").error,
//      expected = Violation(Constraint.numeric.lessThan(reference = 0), actual = 3).some
//    )
//  }
  test("text.length") {
    assertEquals(obtained = text.length.run(""), expected = 0.valid)
    assertEquals(obtained = text.length.run("foobar"), expected = 6.valid)
  }

  test("text.nonEmpty") {
    assertEquals(
      obtained = text.nonEmpty.run("foobar"),
      expected = ().valid
    )
    assertEquals(
      obtained = text.nonEmpty.run(""),
      expected = constraints.text.atLeast(reference = 1).toViolation(actual = "").invalidNec
    )
  }

  test("text.required") {
    assertEquals(obtained = text.required.run("foobar"), expected = "foobar".valid)
    assertEquals(obtained = text.required.run(" foo  bar  "), expected = "foo  bar".valid)
    assertEquals(
      obtained = text.required.run(""),
      expected = constraints.text.required.toViolation(actual = "").invalidNec
    )
  }

  test("text.trim") {
    assertEquals(obtained = text.trim.run(""), expected = "".valid)
    assertEquals(obtained = text.trim.run(" foo  bar   "), expected = "foo  bar".valid)
  }

//  test("text.exactly") {
//    assert(text.exactly(reference = 3).run("foo").isValid)
//    assert(text.exactly(reference = 0).run("").isValid)
//    assertEquals(
//      obtained = text.exactly(reference = 1).run("foo").error,
//      expected = Violation(Constraint.numeric.equal(reference = 1, delta = 0), actual = 3).some
//    )
//  }
//
//  test("text.matches") {
//    val Whitespace = "\\s+".r
//
//    assert(text.matches(regex = Whitespace).run("   ").isValid)
//    assertEquals(
//      obtained = text.matches(regex = Whitespace).run(" foobar ").error,
//      expected = Violation(Constraint.text.matches(regex = Whitespace), actual = " foobar ").some
//    )
//    assertEquals(
//      obtained = text.matches(regex = Whitespace).run("").error,
//      expected = Violation(Constraint.text.matches(regex = Whitespace), actual = "").some
//    )
//  }
//
//  test("toDebugString") {
//    val validation = text.required
//      .andThen(text.atLeast(reference = 3, equal = false) and text.atMost(reference = 10))
//
//    assertEquals(
//      obtained = validation.toDebugString,
//      expected =
//        "[greaterThan(reference=Int(1), delta=0, equal=true), greaterThan(reference=Int(3), delta=0, equal=false), lessThan(reference=Int(10), delta=0, equal=true)]"
//    )
//  }
