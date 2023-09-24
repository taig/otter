package io.taig.otter.schema

import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.schema.schemas.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violation
import munit.FunSuite

import scala.collection.immutable.VectorMap

final class FieldTest extends FunSuite:
  test("decode"):
    assertEquals(
      obtained = field("foo", int).decodeWithRemainders(VectorMap("foo" := 42)),
      expected = (VectorMap.empty, 42).valid
    )

  test("decode: remainder"):
    assertEquals(
      obtained = field("foo", int).decodeWithRemainders(
        VectorMap("foo" := 42, "bar" := true)
      ),
      expected = (VectorMap("bar" := true), 42).valid
    )

  test("decode: violations"):
    assertEquals(
      obtained = field("foo", int).decodeWithRemainders(VectorMap.empty),
      expected = Violations.rootNec(Violation.required).invalid
    )

  test("encode"):
    assertEquals(
      obtained = field("foo", int).encode(42, Null.Show),
      expected = OpenApi.obj("foo" := 42)
    )

  test("encode: show nulls"):
    assertEquals(
      obtained = field("foo", int.optional).nulls.show.encode(42.some, Null.Show),
      expected = OpenApi.obj("foo" := 42)
    )
    assertEquals(
      obtained = field("foo", int.optional).nulls.show.encode(none, Null.Show),
      expected = OpenApi.obj("foo" -> OpenApi.Null)
    )

  test("encode: hide nulls"):
    assertEquals(
      obtained = field("foo", int.optional).nulls.hide.encode(42.some, Null.Show),
      expected = OpenApi.obj("foo" := 42)
    )
    assertEquals(
      obtained = field("foo", int.optional).nulls.hide.encode(none, Null.Show),
      expected = OpenApi.Object.Empty
    )

  test("encode: inherit nulls"):
    assertEquals(
      obtained = field("foo", int.optional).nulls.inherit.encode(none, Null.Show),
      expected = OpenApi.obj("foo" -> OpenApi.Null)
    )
    assertEquals(
      obtained = field("foo", int.optional).nulls.inherit.encode(none, Null.Hide),
      expected = OpenApi.Object.Empty
    )
