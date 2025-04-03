package io.taig.otter

import io.taig.otter.Keys.*
import io.taig.otter.JsonDsl.*
import io.taig.otter.JsonDsl.given
import scala.collection.immutable.ListMap
import cats.data.State
import cats.syntax.all.*

final class JsonZodRendererTest extends OtterSuite:
  val renderer: Renderer[Json[?], State[ListMap[Const, String], Expression]] = JsonZodRenderer()

  test("override"):
    assertEq(
      obtained = renderer(string.modifyMetadata(_.put(typescript, "z.foobar()"))).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.foobar()""")
    )

  test("override: reference"):
    assertEq(
      obtained =
        renderer(string.modifyMetadata(_.put(typescript, "z.foobar()").put(name, "Foo"))).runA(ListMap.empty).value,
      expected = Expression.Referenced(reference = Const(namespace = none, name = "Foo"), value = """z.foobar()""")
    )

  test("collection"):
    assertEq(
      obtained = renderer(collection.list(string)).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.array(z.string())""")
    )

  test("collection: reference"):
    val codec = string.modifyMetadata(_.put(name, "Foo"))

    assertEq(
      obtained = renderer(collection.list(codec)).run(ListMap.empty).value,
      expected = (
        ListMap((Const(namespace = none, name = "Foo"), "z.string()")),
        Expression.Inline("""z.array(Foo)""")
      )
    )

  test("constant"):
    assertEq(
      obtained = renderer(constant("foobar")).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.literal("foobar")""")
    )

  test("dictionary"):
    assertEq(
      obtained = renderer(dictionary.list(key = string, value = long)).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.record(z.string(), z.number())""")
    )

  test("enumeration"):
    enum Animal:
      case Bird
      case Cat
      case Dog

    val codec: Json.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.enum(["bird", "cat", "dog"])""")
    )

  test("optional"):
    assertEq(
      obtained = renderer(string.nullable).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.nullable(z.string())""")
    )
    assertEq(
      obtained = renderer(string.nullable(default = "foobar")).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.nullable(z.string())""")
    )

  test("optional: reference"):
    val codec = string.modifyMetadata(_.put(name, "Foo"))

    assertEq(
      obtained = renderer(codec.nullable).runA(ListMap.empty).value,
      expected = Expression.Inline("""z.nullable(Foo)""")
    )

  test("primitive"):
    assertEq(
      obtained = renderer(string).runA(ListMap.empty).value,
      expected = Expression.Inline("z.string()")
    )
    assertEq(
      obtained = renderer(int).runA(ListMap.empty).value,
      expected = Expression.Inline("z.number()")
    )
    assertEq(
      obtained = renderer(long).runA(ListMap.empty).value,
      expected = Expression.Inline("z.number()")
    )
    assertEq(
      obtained = renderer(boolean).runA(ListMap.empty).value,
      expected = Expression.Inline("z.boolean()")
    )

  // test("record"):
  //   val codec = field("foo", string).toRecord // summon[FieldInvariant[Json.Key, Json, Json.Record]].:*(field("foo", string))(field("bar", int))
