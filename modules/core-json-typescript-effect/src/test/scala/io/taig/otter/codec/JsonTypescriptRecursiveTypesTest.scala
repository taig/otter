package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect
import io.taig.otter.TypescriptKeys
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

object JsonTypescriptRecursiveTypesTest extends ZIOSpecDefault:
  final case class Tree(value: Int, children: List[JsonTypescriptRecursiveTypesTest.Tree])
  final case class OptionalTree(value: Option[Int], children: List[JsonTypescriptRecursiveTypesTest.OptionalTree])

  final case class BooleanTree(value: Boolean, children: List[JsonTypescriptRecursiveTypesTest.BooleanTree])
  final case class Parent(value: Int, children: List[JsonTypescriptRecursiveTypesTest.Child])
  final case class Child(children: List[JsonTypescriptRecursiveTypesTest.Parent])

  private lazy val tree: Json.Record[JsonTypescriptRecursiveTypesTest.Tree] =
    (field("value", coerce(int)) :* field("children", collection.list(tree)))
      .to[JsonTypescriptRecursiveTypesTest.Tree]
      .attr(Keys.name, "Tree")

  private lazy val optionalTree: Json.Record[JsonTypescriptRecursiveTypesTest.OptionalTree] =
    (field("value", int).optional :* field("children", collection.list(optionalTree)))
      .to[JsonTypescriptRecursiveTypesTest.OptionalTree]
      .attr(Keys.name, "OptionalTree")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptRecursiveTypesTest")(
    test("recursive coercion separates decoded and encoded types"):
      val source = JsonTypescriptEffectRenderer.reader.render(tree).mkString("\n")

      assertTrue(
        source.contains("export type TreeEncoded ="),
        source.contains("\"value\": number;"),
        source.contains("\"value\": number | string;"),
        source.contains("ReadonlyArray<TreeEncoded>"),
        source.contains("Schema.Schema<Tree, TreeEncoded>")
      )
    ,
    test("optional nullable fields retain null only in the encoded projection"):
      val source = JsonTypescriptEffectRenderer.reader.render(optionalTree).mkString("\n")

      assertTrue(
        source.contains("\"value\"?: number | undefined;"),
        source.contains("\"value\"?: number | null | undefined;"),
        source.contains("ReadonlyArray<OptionalTreeEncoded>"),
        source.contains("Schema.Schema<OptionalTree, OptionalTreeEncoded>")
      )
    ,
    test("the symmetric write side remains compact"):
      val source = JsonTypescriptEffectRenderer.writer.render(tree).mkString("\n")

      assertTrue(source.contains("Schema.Schema<Tree>"), !source.contains("TreeEncoded"))
    ,
    test("encoded type names cannot overwrite explicitly named schemas"):
      val named = string.attr(Keys.name, "TreeEncoded")
      val source =
        JsonTypescriptEffectRenderer.reader.render(field("named", named) :* field("tree", tree)).mkString("\n")

      assertTrue(
        source.contains("export const TreeEncoded = Schema.String;"),
        source.contains("export type TreeEncoded_2 ="),
        source.contains("Schema.Schema<Tree, TreeEncoded_2>"),
        source.contains("ReadonlyArray<TreeEncoded_2>")
      )
    ,
    test("a decoded type override does not replace the wire type"):
      val date = string
        .attr(TypescriptKeys.expression, TypescriptEffect.symbol("Date"))
        .attr(TypescriptKeys.tpe, Typescript.Type.Symbol("Date", Nil))
        .attr(Keys.name, "Timestamp")
      val source = JsonTypescriptEffectRenderer.reader.render(date).mkString("\n")

      assertTrue(
        source.contains("export type Timestamp = Date;"),
        source.contains("export type TimestampEncoded = string;"),
        source.contains("Schema.Schema<Timestamp, TimestampEncoded>")
      )
    ,
    test("an encoded type override describes a custom transformation"):
      val transformed = int
        .attr(TypescriptKeys.expression, TypescriptEffect.symbol("NumberFromString"))
        .attr(TypescriptKeys.encodedType, Typescript.Type.Symbol("string", Nil))
        .attr(Keys.name, "Number")
      val source = JsonTypescriptEffectRenderer.reader.render(transformed).mkString("\n")

      assertTrue(
        source.contains("export type Number_2 = number;"),
        source.contains("export type Number_2Encoded = string;"),
        source.contains("Schema.Schema<Number_2, Number_2Encoded> = Schema.NumberFromString;")
      )
    ,
    test("recursive boolean coercion names only the strings the Effect schema accepts"):
      lazy val booleans: Json.Record[JsonTypescriptRecursiveTypesTest.BooleanTree] =
        (field("value", coerce(boolean)) :* field("children", collection.list(booleans)))
          .to[JsonTypescriptRecursiveTypesTest.BooleanTree]
          .attr(Keys.name, "Tree")
      val source = JsonTypescriptEffectRenderer.reader.render(booleans).mkString("\n")

      assertTrue(
        source.contains("\"value\": boolean;"),
        source.contains("\"value\": boolean | \"true\" | \"false\";"),
        source.contains("Schema.Schema<Tree, TreeEncoded>")
      )
    ,
    test("nullable values remain nullable after decoding"):
      lazy val nullable: Json.Record[JsonTypescriptRecursiveTypesTest.OptionalTree] =
        (field("value", int.optional) :* field("children", collection.list(nullable)))
          .to[JsonTypescriptRecursiveTypesTest.OptionalTree]
          .attr(Keys.name, "Tree")
      val source = JsonTypescriptEffectRenderer.reader.render(nullable).mkString("\n")

      assertTrue(source.contains("\"value\": number | null;"), source.contains("Schema.Schema<Tree>"))
    ,
    test("mutual recursion follows the encoded type through an inferred declaration"):
      lazy val parent: Json.Record[JsonTypescriptRecursiveTypesTest.Parent] =
        (field("value", coerce(int)) :* field("children", collection.list(child)))
          .to[JsonTypescriptRecursiveTypesTest.Parent]
          .attr(Keys.name, "Parent")
      lazy val child: Json.Record[JsonTypescriptRecursiveTypesTest.Child] =
        field("children", collection.list(parent)).toRecord
          .to[JsonTypescriptRecursiveTypesTest.Child]
          .attr(Keys.name, "Child")
      val source = JsonTypescriptEffectRenderer.module(parent).mkString("\n")

      assertTrue(
        source.contains("Schema.Schema.Encoded<typeof ParentRead>"),
        source.contains("Schema.Schema<ChildRead, ChildReadEncoded>"),
        source.contains("export type ParentRead = Schema.Schema.Type<typeof ParentRead>;"),
        source.contains("export const ParentWrite = Schema.Struct(")
      )
  )
