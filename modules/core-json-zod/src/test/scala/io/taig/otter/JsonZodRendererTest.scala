package io.taig.otter

import io.taig.otter.Keys.*
import io.taig.otter.JsonDsl.*
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

  test("record"):
    val codec = field("foo", string) :* field("bar", int)

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.object({
          |  "foo": z.string(),
          |  "bar": z.number()
          |})""".stripMargin
      )
    )

  test("record: optional"):
    val codec = field("foo", string) :* field("bar", int).optional

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.object({
          |  "foo": z.string(),
          |  "bar": z.optional(z.number())
          |})""".stripMargin
      )
    )

  test("tuple"):
    val codec = string :* int

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.tuple([
          |  z.string(),
          |  z.number()
          |])""".stripMargin
      )
    )

  test("union: untagged"):
    val codec = branch("foo", string) :+ branch("bar", int)

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.union([
          |  z.string(),
          |  z.number()
          |])""".stripMargin
      )
    )

  test("union: taggged (keyed)"):
    val codec = (branch("foo", string) :+ branch("bar", int)).keyed

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.union([
          |  z.object({
          |    "foo": z.string()
          |  }),
          |  z.object({
          |    "bar": z.number()
          |  })
          |])""".stripMargin
      )
    )

  test("union: taggged (merged)"):
    val codec = (branch("foo", field("x", int)) :+ branch("bar", field("y", long))).merged

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.union([
          |  z.object({
          |    "x": z.number()
          |  }).merge(z.object({ "type": z.literal("foo") })),
          |  z.object({
          |    "y": z.number()
          |  }).merge(z.object({ "type": z.literal("bar") }))
          |])""".stripMargin
      )
    )

  test("union: taggged (explicit)"):
    val codec = (branch("foo", string) :+ branch("bar", int)).explicit

    assertEq(
      obtained = renderer(codec).runA(ListMap.empty).value,
      expected = Expression.Inline(
        """z.union([
          |  z.object({
          |    "type": z.literal("foo"),
          |    "value": z.string()
          |  }),
          |  z.object({
          |    "type": z.literal("bar"),
          |    "value": z.number()
          |  })
          |])""".stripMargin
      )
    )
