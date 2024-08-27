package io.taig.otter.http.header

import munit.FunSuite
import cats.syntax.all.*
import org.typelevel.ci.*

final class MediaTypeTest extends FunSuite:
  test("parse"):
    assertEquals(
      obtained = MediaType.parse("text/html"),
      expected = MediaType(tpe = MediaType.Type("text", "html"), parameters = Parameters.Empty).asRight
    )

    assertEquals(
      obtained = MediaType.parse("image/png"),
      expected = MediaType(tpe = MediaType.Type("image", "png"), parameters = Parameters.Empty).asRight
    )

    assertEquals(
      obtained = MediaType.parse("text/plain; charset=utf-8"),
      expected = MediaType(
        tpe = MediaType.Type("text", "plain"),
        parameters = Parameters.of(ci"charset" -> "utf-8")
      ).asRight
    )

    assertEquals(
      obtained = MediaType.parse("text/plain; charset=\"utf-8\""),
      expected = MediaType(
        tpe = MediaType.Type("text", "plain"),
        parameters = Parameters.of(ci"charset" -> "utf-8")
      ).asRight
    )

    assertEquals(
      obtained = MediaType.parse("text/plain;charset=utf-8;foo=bar"),
      expected = MediaType(
        tpe = MediaType.Type("text", "plain"),
        parameters = Parameters.of(ci"charset" -> "utf-8", ci"foo" -> "bar")
      ).asRight
    )

  test("show"):
    assertEquals(
      obtained = MediaType(tpe = MediaType.Type("text", "html"), parameters = Parameters.Empty).show,
      expected = "text/html"
    )

    assertEquals(
      obtained = MediaType(
        tpe = MediaType.Type("application", "json"),
        parameters = Parameters.of(ci"charset" -> "utf-8")
      ).show,
      expected = "application/json; charset=\"utf-8\""
    )

    assertEquals(
      obtained = MediaType(
        tpe = MediaType.Type("application", "json"),
        parameters = Parameters.of(ci"charset" -> "utf-8", ci"foo" -> "bar")
      ).show,
      expected = "application/json; charset=\"utf-8\"; foo=\"bar\""
    )
