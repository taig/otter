package io.taig.openapi.http

import munit.FunSuite
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.Void

import java.util.UUID

final class UrlTest extends FunSuite:
  val url: Url[(String, Int, Long, Option[UUID], String)] =
    __ / "foo" / parameter("a", string) / "bar" / parameter("b", int)
      & query("x", long)
      & query("y", uuid).optional
      & query("z", string)

  test("matches") {}
