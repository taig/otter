package io.taig.otter.http.header

import munit.FunSuite
import cats.syntax.all.*
import org.typelevel.ci.*

final class ContentTypeTest extends FunSuite:
  test("parse"):
    assertEquals(
      obtained = ContentType.parse("text/html"),
      expected = ContentType(tpe = "text", subtype = "html", parameters = Nil).asRight
    )

    assertEquals(
      obtained = ContentType.parse("image/png"),
      expected = ContentType(tpe = "image", subtype = "png", parameters = Nil).asRight
    )

    assertEquals(
      obtained = ContentType.parse("text/plain; charset=utf-8"),
      expected = ContentType(
        tpe = "text",
        subtype = "plain",
        parameters = List(Parameter(ci"charset", "utf-8"))
      ).asRight
    )

    assertEquals(
      obtained = ContentType.parse("text/plain; charset=\"utf-8\""),
      expected = ContentType(
        tpe = "text",
        subtype = "plain",
        parameters = List(Parameter(ci"charset", "utf-8"))
      ).asRight
    )

    assertEquals(
      obtained = ContentType.parse("text/plain;charset=utf-8;foo=bar"),
      expected = ContentType(
        tpe = "text",
        subtype = "plain",
        parameters = List(Parameter(ci"charset", "utf-8"), Parameter(ci"foo", "bar"))
      ).asRight
    )

  test("show"):
    assertEquals(
      obtained = ContentType(tpe = "text", subtype = "html", parameters = Nil).show,
      expected = "text/html"
    )

    assertEquals(
      obtained = ContentType(
        tpe = "application",
        subtype = "json",
        parameters = List(Parameter(ci"charset", "utf-8"))
      ).show,
      expected = "application/json; charset=\"utf-8\""
    )

    assertEquals(
      obtained = ContentType(
        tpe = "application",
        subtype = "json",
        parameters = List(Parameter(ci"charset", "utf-8"), Parameter(ci"foo", "bar"))
      ).show,
      expected = "application/json; charset=\"utf-8\"; foo=\"bar\""
    )
