package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.OtterSuite
import io.taig.otter.http.codec.HttpQueryDecoder
import io.taig.otter.http.component.HttpQueryComponent.*

final class HttpQueryParserTest extends OtterSuite:
  test("primitive"):
    assertEq(
      obtained = HttpQueryDecoder(explode = false, style = Query.Style.Form).decodeRemainding(
        schema = string,
        values = Chain("foo".some, "bar".some)
      ),
      expected = (Chain("bar".some), "foo").valid
    )

  // test("primitive: remainders"):
  //   assertEq(
  //     obtained = HttpQueryDecoder(explode = false, style = Query.Style.Form).decode(

  //       schema = string,
  //       values = Chain(("x", none), ("foo", "bar".some), ("a", "b".some))
  //     ),
  //     expected = (Chain(("x", none), ("a", "b".some)), "bar").valid
  //   )

  // test("optional: primitive"):
  //   assertEq(
  //     obtained = HttpQueryDecoder(explode = false, style = Query.Style.Form).decode(

  //       schema = nullable(string),
  //       values = Chain(("foo", "bar".some))
  //     ),
  //     expected = (Chain.empty, "bar".some).valid
  //   )
  //   assertEq(
  //     obtained = HttpQueryDecoder(explode = false, style = Query.Style.Form).decode(

  //       schema = nullable(string),
  //       values = Chain(("foo", none))
  //     ),
  //     expected = (Chain.empty, none).valid
  //   )
  //   assertEq(
  //     obtained = HttpQueryDecoder(explode = false, style = Query.Style.Form).decode(

  //       schema = nullable(string, "bar"),
  //       values = Chain(("foo", "baz".some))
  //     ),
  //     expected = (Chain.empty, "baz").valid
  //   )
  //   assertEq(
  //     obtained = HttpQueryDecoder(explode = false, style = Query.Style.Form).decode(
  //       name = "foo",
  //       codec = nullable(string, "bar"),
  //       values = Chain(("foo", none))
  //     ),
  //     expected = (Chain.empty, "bar").valid
  //   )
