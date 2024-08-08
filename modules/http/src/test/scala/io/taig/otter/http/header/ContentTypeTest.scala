package io.taig.otter.http.header

import munit.FunSuite
import cats.syntax.all.*

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
        parameters = List(ContentType.Parameter("charset", "utf-8"))
      ).asRight
    )

    assertEquals(
      obtained = ContentType.parse("text/plain; charset=\"utf-8\""),
      expected = ContentType(
        tpe = "text",
        subtype = "plain",
        parameters = List(ContentType.Parameter("charset", "utf-8"))
      ).asRight
    )

    assertEquals(
      obtained = ContentType.parse("text/plain;charset=utf-8;foo=bar"),
      expected = ContentType(
        tpe = "text",
        subtype = "plain",
        parameters = List(ContentType.Parameter("charset", "utf-8"), ContentType.Parameter("foo", "bar"))
      ).asRight
    )

  test("print"):
    assertEquals(
      obtained = ContentType(tpe = "text", subtype = "html", parameters = Nil).print,
      expected = "text/html"
    )

    assertEquals(
      obtained = ContentType(
        tpe = "application",
        subtype = "json",
        parameters = List(ContentType.Parameter("charset", "utf-8"))
      ).print,
      expected = "application/json; charset=\"utf-8\""
    )

    assertEquals(
      obtained = ContentType(
        tpe = "application",
        subtype = "json",
        parameters = List(
          ContentType.Parameter("charset", "utf-8"),
          ContentType.Parameter("foo", "bar")
        )
      ).print,
      expected = "application/json; charset=\"utf-8\"; foo=\"bar\""
    )
