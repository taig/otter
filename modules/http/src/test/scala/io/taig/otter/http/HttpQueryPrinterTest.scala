package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.OtterSuite
import io.taig.otter.http.HttpQueryDsl.*

final class HttpQueryPrinterTest extends OtterSuite:
  test("collection: style = form, explode = true"):
    assertEq(
      obtained = HttpQueryPrinter(explode = true, style = Query.Style.Form)(
        name = "foo",
        codec = collection.list(string),
        List("bar", "baz")
      ),
      expected = Chain(("foo", "bar".some), ("foo", "baz".some))
    )

  test("collection: style = form, explode = false"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = collection.list(string),
        List("bar", "baz")
      ),
      expected = Chain.one(("foo", "bar,baz".some))
    )

  test("collection: style = form, explode = false (escape)"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = collection.list(string),
        List("foo,bar", "baz")
      ),
      expected = Chain.one(("foo", "foo\\,bar,baz".some))
    )

  test("collection: style = spaceDelimited, explode = true"):
    assertEq(
      obtained = HttpQueryPrinter(explode = true, style = Query.Style.SpaceDelimited)(
        name = "foo",
        codec = collection.list(string),
        List("bar", "baz")
      ),
      expected = Chain(("foo", "bar".some), ("foo", "baz".some))
    )

  test("collection: style = spaceDelimited, explode = false"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.SpaceDelimited)(
        name = "foo",
        codec = collection.list(string),
        List("bar", "baz")
      ),
      expected = Chain.one(("foo", "bar baz".some))
    )

  test("collection: style = spaceDelimited, explode = false (escape)"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.SpaceDelimited)(
        name = "foo",
        codec = collection.list(string),
        List("foo bar", "baz")
      ),
      expected = Chain.one(("foo", "foo\\ bar baz".some))
    )

  test("collection: style = pipeDelimited, explode = true"):
    assertEq(
      obtained = HttpQueryPrinter(explode = true, style = Query.Style.PipeDelimited)(
        name = "foo",
        codec = collection.list(string),
        List("bar", "baz")
      ),
      expected = Chain(("foo", "bar".some), ("foo", "baz".some))
    )

  test("collection: style = pipeDelimited, explode = false"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.PipeDelimited)(
        name = "foo",
        codec = collection.list(string),
        List("bar", "baz")
      ),
      expected = Chain.one(("foo", "bar|baz".some))
    )

  test("collection: style = pipeDelimited, explode = false (escape)"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.PipeDelimited)(
        name = "foo",
        codec = collection.list(string),
        List("foo|bar", "baz")
      ),
      expected = Chain.one(("foo", "foo\\|bar|baz".some))
    )

  test("dictionary: style = form, explode = true"):
    assertEq(
      obtained = HttpQueryPrinter(explode = true, style = Query.Style.Form)(
        name = "foo",
        codec = dictionary.list(string, string),
        List(("a", "b"), ("x", "y"))
      ),
      expected = Chain(("a", "b".some), ("x", "y".some))
    )

  test("dictionary: style = form, explode = false"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.Form)(
        name = "foo",
        codec = dictionary.list(string, string),
        List(("a,b", "c"), ("x", "y,z"))
      ),
      expected = Chain.one(("foo", "a\\,b,c,x,y\\,z".some))
    )

  test("primitive"):
    assertEq(
      obtained = HttpQueryPrinter(explode = false, style = Query.Style.Form)(name = "foo", codec = string, "bar"),
      expected = Chain.one(("foo", "bar".some))
    )
