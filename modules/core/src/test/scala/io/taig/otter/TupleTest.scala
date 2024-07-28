package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Dsl.*
import munit.FunSuite

final class TupleTest extends FunSuite:
  test("encode"):
    val codec = tuple(field("foo", string) :* field("bar", int))

    assertEquals(
      obtained = codec.encode(("foobar", 42)),
      expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
    )

  test("encode: optional"):
    val codec = tuple(field("foo", string) :* field("bar", int)).optional

    assertEquals(
      obtained = codec.encode(("foobar", 42).some),
      expected = Data.Array.of(Data.String("foobar"), Data.Number(42))
    )
    assertEquals(
      obtained = codec.encode(none),
      expected = Data.Null
    )

  test("encode: optional (product)"):
    val codec = tuple(field("foo", string) :* field("bar", int))
    val product = codec.optional.zip(codec.optional)

    assertEquals(
      obtained = product.encode(("foobar", 42).some, ("foobar", 42).some),
      expected = Data.Array.of(Data.String("foobar"), Data.Number(42), Data.String("foobar"), Data.Number(42))
    )

    assertEquals(
      obtained = product.encode(none, ("foobar", 42).some),
      expected = Data.Array.of(Data.Null, Data.Null, Data.String("foobar"), Data.Number(42))
    )

    assertEquals(
      obtained = product.encode(("foobar", 42).some, none),
      expected = Data.Array.of(Data.String("foobar"), Data.Number(42), Data.Null, Data.Null)
    )

    assertEquals(
      obtained = product.encode(none, none),
      expected = Data.Array.of(Data.Null, Data.Null, Data.Null, Data.Null)
    )
