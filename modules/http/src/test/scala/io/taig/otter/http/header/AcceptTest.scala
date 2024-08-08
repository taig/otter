package io.taig.otter.http.header

import munit.FunSuite
import cats.data.NonEmptyList
import cats.syntax.all.*

final class AcceptTest extends FunSuite:
  test("parse"):
    assertEquals(
      obtained = Accept.parse("text/plain"),
      expected = Accept(
        NonEmptyList.one(
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = Nil
            ),
            weight = none
          )
        )
      ).asRight
    )

    assertEquals(
      obtained = Accept.parse("text/plain, image/png"),
      expected = Accept(
        NonEmptyList.of(
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = Nil
            ),
            weight = none
          ),
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("image", "png"),
              parameters = Nil
            ),
            weight = none
          )
        )
      ).asRight
    )
