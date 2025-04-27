package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.OtterSuite
import io.taig.otter.http.HttpQueryDsl.*

final class HttpQueryParserTest extends OtterSuite:
  test("primitive"):
    assertEq(
      obtained = HttpQueryParser(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = string,
        values = Chain(("foo", "bar".some))
      ),
      expected = (Chain.empty, "bar").valid
    )

  test("primitive: remainders"):
    assertEq(
      obtained = HttpQueryParser(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = string,
        values = Chain(("x", none), ("foo", "bar".some), ("a", "b".some))
      ),
      expected = (Chain(("x", none), ("a", "b".some)), "bar").valid
    )

  test("optional: primitive"):
    assertEq(
      obtained = HttpQueryParser(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = nullable(string),
        values = Chain(("foo", "bar".some))
      ),
      expected = (Chain.empty, "bar".some).valid
    )
    assertEq(
      obtained = HttpQueryParser(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = nullable(string),
        values = Chain(("foo", none))
      ),
      expected = (Chain.empty, none).valid
    )
    assertEq(
      obtained = HttpQueryParser(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = nullable(string, "bar"),
        values = Chain(("foo", "baz".some))
      ),
      expected = (Chain.empty, "baz").valid
    )
    assertEq(
      obtained = HttpQueryParser(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = nullable(string, "bar"),
        values = Chain(("foo", none))
      ),
      expected = (Chain.empty, "bar").valid
    )
