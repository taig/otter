package io.taig.otter

import munit.FunSuite
import io.taig.otter.Dsl.*
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.History

final class CollectionTest extends FunSuite:
  test("encode"):
    val codec = collection.vector(string)

    assertEquals(
      obtained = codec.encode(Vector("foo", "bar", "baz")),
      expected = Data.Array.of(Data.String("foo"), Data.String("bar"), Data.String("baz"))
    )

    assertEquals(obtained = codec.encode(Vector.empty), expected = Data.Array.Empty)

  test("decode"):
    val codec = collection.vector(string)

    assertEquals(
      obtained = codec.decode(Data.Array.of(Data.String("foo"), Data.String("bar"), Data.String("baz"))),
      expected = Vector("foo", "bar", "baz").valid
    )

    assertEquals(
      obtained = codec.decode(Data.Array.of(Data.String("foo"), Data.Array.Empty, Data.Object.Empty)),
      expected = Violations
        .of(
          Violations
            .namespaceNec(
              History.Step.Index(1),
              Violation(Constraint.Type("string"), actual = Data.String("array"))
            ),
          Violations
            .namespaceNec(
              History.Step.Index(2),
              Violation(Constraint.Type("string"), actual = Data.String("object"))
            )
        )
        .invalid
    )

    assertEquals(obtained = codec.decode(Data.Array.Empty), expected = Vector.empty.valid)
