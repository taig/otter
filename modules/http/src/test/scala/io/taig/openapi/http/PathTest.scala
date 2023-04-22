package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import munit.FunSuite
import io.taig.openapi.http.syntax.*
import io.taig.screening.Violation
import io.taig.openapi.{History, OpenApi}
import io.taig.screening.identifiers
import io.taig.screening.syntax.*
import io.taig.openapi.http.schemas.*
import io.taig.openapi.schema.Violations
import io.taig.openapi.schema.schemas.*

final class PathTest extends FunSuite:
  test("/") {
    val _: Path[Unit] = Path.Root / "foo" / "bar"
    val _: Path[String] = Path.Root / parameter("foo", string) / "bar"
    val _: Path[(String, Int)] = Path.Root / parameter("foo", string) / parameter("bar", int)
    val _: Path[((String, Int), Long)] =
      Path.Root / parameter("foo", string) / parameter("bar", int) / parameter("foobar", long)
  }

  test("print") {
    assertEquals(obtained = Path.Root.print, expected = Chain.empty)
    assertEquals(obtained = (Path.Root / "foo" / "bar").print, expected = Chain("foo", "bar"))
    assertEquals(
      obtained = (Path.Root / parameter("foo", int) / parameter("bar", string)).print,
      expected = Chain("{foo}", "{bar}")
    )
    assertEquals(obtained = (Path.Root / "foo" / parameter("bar", string)).print, expected = Chain("foo", "{bar}"))
    assertEquals(obtained = (Path.Root / parameter("foo", int) / "bar").print, expected = Chain("{foo}", "bar"))
  }

  test("encode") {
    assertEquals(obtained = Path.Root.encode(()), expected = Chain.empty)
    assertEquals(
      obtained = (Path.Root / "foo" / "bar").encode(()),
      expected = Chain(OpenApi.fromString("foo"), OpenApi.fromString("bar"))
    )
    assertEquals(
      obtained = (Path.Root / parameter("foo", int) / parameter("bar", string)).encode((42, "foo")),
      expected = Chain(OpenApi.fromInt(42), OpenApi.fromString("foo"))
    )
    assertEquals(
      obtained = (Path.Root / "foo" / parameter("bar", string)).encode("foo"),
      expected = Chain(OpenApi.fromString("foo"), OpenApi.fromString("foo"))
    )
    assertEquals(
      obtained = (Path.Root / parameter("foo", int) / "bar").encode(42),
      expected = Chain(OpenApi.fromInt(42), OpenApi.fromString("bar"))
    )
  }

  test("decode") {
    assertEquals(obtained = Path.Root.decode(Chain.empty), expected = ().valid)
    assertEquals(
      obtained = (Path.Root / "foo" / "bar").decode(Chain("foo", "bar").map(OpenApi.fromString)),
      expected = ().valid
    )
    assertEquals(
      obtained = (Path.Root / "foo" / "bar").decode(Chain("x", "y").map(OpenApi.fromString)),
      expected = Violations
        .oneNec(
          History.Root / "foo",
          identifiers.text.matches
            .toConstraint(reference = OpenApi.fromString("foo").some)
            .toViolation(actual = OpenApi.fromString("x"))
        )
        .invalid
    )
    assertEquals(
      obtained = (Path.Root / "foo" / "bar").decode(Chain("foo", "y").map(OpenApi.fromString)),
      expected = Violations
        .oneNec(
          History.Root / "bar",
          identifiers.text.matches
            .toConstraint(reference = OpenApi.fromString("bar").some)
            .toViolation(actual = OpenApi.fromString("y"))
        )
        .invalid
    )
  }
