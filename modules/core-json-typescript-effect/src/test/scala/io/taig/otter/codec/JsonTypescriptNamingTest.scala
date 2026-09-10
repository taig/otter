package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Tree
import zio.Scope
import zio.test.*

object JsonTypescriptNamingTest extends ZIOSpecDefault:
  private def render(schema: Json.Node[?, ?]): String =
    JsonTypescriptEffectRenderer.writer.render(schema).mkString("\n\n")

  private def constants(declarations: List[Typescript.Statement.Declaration]): List[String] =
    declarations.collect { case Typescript.Statement.Declaration.Constant(_, name, _, _) => name }

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptNamingTest")(
    test("different records requesting the same name keep their own definitions"):
      val left = field("a", int).toRecord.attr(Keys.name, "Shared")
      val right = field("b", string).toRecord.attr(Keys.name, "Shared")
      val schema = field("left", left) :* field("right", right) :* field("again", right)
      val source = render(schema)

      assertTrue(
        source.contains("export const Shared = Schema.Struct({ \"a\": Schema.Int });"),
        source.contains("export const Shared_2 = Schema.Struct({ \"b\": Schema.String });"),
        source.contains("\"left\": Shared,"),
        source.contains("\"right\": Shared_2,"),
        source.contains("\"again\": Shared_2"),
        !source.contains("Schema.suspend"),
        render(schema) == source
      )
    ,
    test("a child sharing its parent's name is not recursion"):
      val child = field("b", string).toRecord.attr(Keys.name, "Shared")
      val parent = field("child", child).toRecord.attr(Keys.name, "Shared")
      val source = render(parent)

      assertTrue(
        source.contains("export const Shared_2 = Schema.Struct({ \"b\": Schema.String });"),
        source.contains("export const Shared = Schema.Struct({ \"child\": Shared_2 });"),
        !source.contains("Schema.suspend")
      )
    ,
    test("allocated names skip names already requested explicitly"):
      val reserved = boolean.attr(Keys.name, "Shared_2")
      val first = int.attr(Keys.name, "Shared")
      val second = string.attr(Keys.name, "Shared")
      val source = render(field("reserved", reserved) :* field("first", first) :* field("second", second))

      assertTrue(
        source.contains("export const Shared_2 = Schema.Boolean;"),
        source.contains("export const Shared_3 = Schema.String;")
      )
    ,
    test("a recursive schema with a colliding name uses its allocated name in values and types"):
      val first = int.attr(Keys.name, "Tree")
      lazy val tree: Json.Record[Tree] =
        (field("value", int) :* field("children", collection.list(tree))).to[Tree].attr(Keys.name, "Tree")
      val source = render(field("first", first) :* field("tree", tree))

      assertTrue(
        source.contains("export const Tree = Schema.Int;"),
        source.contains("export const Tree_2: Schema.Schema<Tree_2>"),
        source.contains("ReadonlyArray<Tree_2>"),
        source.contains("Schema.suspend(() => Tree_2)")
      )
    ,
    test("coercion helpers and user definitions cannot overwrite each other"):
      val named = string.attr(Keys.name, "CoerceNumber")
      val schema = field("label", named) :* field("count", coerce(int)) :* field("again", coerce(int))
      val source = JsonTypescriptEffectRenderer.reader.render(schema).mkString("\n")

      assertTrue(
        source.contains("export const CoerceNumber = Schema.String;"),
        source.contains("export const CoerceNumber_2 = Schema.Union"),
        source.contains("\"count\": CoerceNumber_2.pipe"),
        source.contains("\"again\": CoerceNumber_2.pipe"),
        !source.contains("CoerceNumber_3")
      )
    ,
    test("a single-side module keeps distinct schema instances and reuses shared instances"):
      val first = int.attr(Keys.name, "Shared")
      val second = int.attr(Keys.name, "Shared")

      assertTrue(
        constants(JsonTypescriptEffectRenderer.module(Side.Write, first, second, second)) == List("Shared", "Shared_2")
      )
    ,
    test("collapsing read and write definitions splits the right colliding schema"):
      val first = int.attr(Keys.name, "Shared")
      val second = field("value", int).optional.toRecord.attr(Keys.name, "Shared")
      val module = JsonTypescriptEffectRenderer.module(first, second)
      val source = module.mkString("\n")

      assertTrue(
        constants(module) == List("Shared", "Shared_2Read", "Shared_2Write"),
        source.contains("Schema.optionalWith(Schema.Int, { \"nullable\": true })"),
        source.contains("Schema.optional(Schema.Int)")
      )
    ,
    test("read and write suffixes cannot collide with another schema's requested name"):
      val first = field("value", int).optional.toRecord.attr(Keys.name, "Shared")
      val second = string.attr(Keys.name, "SharedRead")
      val module = JsonTypescriptEffectRenderer.module(first, second)
      val names = constants(module)

      assertTrue(names.distinct == names, names.size == 3, module.mkString("\n").contains("Schema.String"))
  )
