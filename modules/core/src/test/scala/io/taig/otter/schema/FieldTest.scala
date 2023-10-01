package io.taig.otter.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.schemas.*
import io.taig.otter.validation.{Violation, Violations}
import io.taig.otter.{Data, Null}
import munit.FunSuite

final class FieldTest extends FunSuite:
  test("decode"):
    assertEquals(
      obtained = field("foo", int).decodeWithRemainders(Chain("foo" -> Data.Number(42))),
      expected = (Chain.empty, 42).valid
    )

  test("decode: remainder"):
    assertEquals(
      obtained = field("foo", int).decodeWithRemainders(
        Chain("foo" -> Data.Number(42), "bar" -> Data.Boolean(true))
      ),
      expected = (Chain("bar" -> Data.Boolean(true)), 42).valid
    )

  test("decode: violations"):
    assertEquals(
      obtained = field("foo", int).decodeWithRemainders(Chain.empty),
      expected = Violations.rootNec(Violation.required).invalid
    )

  test("encode"):
    assertEquals(
      obtained = field("foo", int).encode(42, Null.Show),
      expected = Chain("foo" -> Data.Number(42))
    )

  test("encode: show nulls"):
    assertEquals(
      obtained = field("foo", int.optional).nulls(Null.Show).encode(42.some, Null.Show),
      expected = Chain("foo" -> Data.Number(42))
    )
    assertEquals(
      obtained = field("foo", int.optional).nulls(Null.Show).encode(none, Null.Show),
      expected = Chain("foo" -> Data.Null)
    )

  test("encode: hide nulls"):
    assertEquals(
      obtained = field("foo", int.optional).nulls(Null.Hide).encode(42.some, Null.Show),
      expected = Chain("foo" -> Data.Number(42))
    )
    assertEquals(
      obtained = field("foo", int.optional).nulls(Null.Hide).encode(none, Null.Show),
      expected = Chain.empty
    )

  test("encode: inherit nulls"):
    assertEquals(
      obtained = field("foo", int.optional).nulls(None).encode(none, Null.Show),
      expected = Chain("foo" -> Data.Null)
    )
    assertEquals(
      obtained = field("foo", int.optional).nulls(None).encode(none, Null.Hide),
      expected = Chain.empty
    )
