package io.taig.otter.http.header

import munit.FunSuite
import cats.data.NonEmptyList
import cats.syntax.all.*
import org.typelevel.ci.*

final class AcceptTest extends FunSuite:
  test("parse"):
    assertEquals(
      obtained = Accept.parse("text/plain"),
      expected = Accept(
        NonEmptyList.of(
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

    assertEquals(
      obtained = Accept.parse("text/plain; q=1"),
      expected = Accept(
        NonEmptyList.of(
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = Nil
            ),
            weight = BigDecimal(1).some
          )
        )
      ).asRight
    )

  test("parse: last q"):
    assertEquals(
      obtained = Accept.parse("text/plain; q=0.5; q=0.7"),
      expected = Accept(
        NonEmptyList.of(
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = List(Parameter(ci"q", "0.5"))
            ),
            weight = BigDecimal("0.7").some
          )
        )
      ).asRight
    )

  test("parse: invalid q"):
    assertEquals(
      obtained = Accept.parse("text/plain; q=0.5; q=foo; q=1.1; q=0.1234"),
      expected = Accept(
        NonEmptyList.of(
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = List(
                Parameter(ci"q", "foo"),
                Parameter(ci"q", "1.1"),
                Parameter(ci"q", "0.1234")
              )
            ),
            weight = BigDecimal("0.5").some
          )
        )
      ).asRight
    )

  test("toSortedList"):
    println(
      Accept(
        NonEmptyList.of(
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Primary("image"), parameters = Nil),
            weight = none
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = Nil),
            weight = none
          ),
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = List(Parameter(ci"foo", "bar"))
            ),
            weight = none
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Secondary("application", "json"), parameters = Nil),
            weight = BigDecimal("0.5").some
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Secondary("text", "html"), parameters = Nil),
            weight = BigDecimal(0).some
          )
        )
      ).toSortedList.map(_.show).mkString(", ")
    )

    assertEquals(
      obtained = Accept(
        NonEmptyList.of(
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Primary("image"), parameters = Nil),
            weight = none
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Primary("text"), parameters = Nil),
            weight = none
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = Nil),
            weight = none
          ),
          Weighted(
            self = MediaRange(
              tpe = MediaRange.Type.Secondary("text", "plain"),
              parameters = List(Parameter(ci"foo", "bar"))
            ),
            weight = none
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Secondary("application", "json"), parameters = Nil),
            weight = BigDecimal("0.5").some
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Secondary("text", "html"), parameters = Nil),
            weight = BigDecimal(0).some
          ),
          Weighted(
            self = MediaRange(tpe = MediaRange.Type.Any, parameters = Nil),
            weight = BigDecimal("0.1").some
          )
        )
      ).toSortedList,
      expected = List(
        MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = List(Parameter(ci"foo", "bar"))),
        MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = Nil),
        MediaRange(tpe = MediaRange.Type.Primary("image"), parameters = Nil),
        MediaRange(tpe = MediaRange.Type.Primary("text"), parameters = Nil),
        MediaRange(tpe = MediaRange.Type.Secondary("application", "json"), parameters = Nil),
        MediaRange(tpe = MediaRange.Type.Any, parameters = Nil)
      )
    )
