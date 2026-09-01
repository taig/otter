package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

/** Both sides of a schema in one module, which is what a client needs: it has to know what it may send as well as what
  * it must be ready to receive.
  */
object JsonTypescriptEffectModuleTest extends ZIOSpecDefault:
  private val note = json.nullableTag.attr(Keys.name, "Note")

  private val book = json.book.attr(Keys.name, "Book")

  private def module(schemas: Json.Node[?, ?]*): String =
    JsonTypescriptEffectRenderer.module(schemas*).mkString("\n\n")

  private def suffixed(schemas: Json.Node[?, ?]*): String =
    JsonTypescriptEffectRenderer.module(JsonTypescriptEffectRenderer.Naming.Suffixed, schemas*).mkString("\n\n")

  private def names(schemas: Json.Node[?, ?]*): List[String] = JsonTypescriptEffectRenderer
    .module(schemas*)
    .collect { case Typescript.Statement.Declaration.Type(_, name, _) => name }

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptEffectModuleTest")(
    /** Nothing in the schema is asymmetric, so both sides came to the same declaration and one name says it. */
    test("a symmetric schema is named once"):
      assertTrue(module(book) == """export type Book = Schema.Schema.Type<typeof Book>;
                                   |
                                   |export const Book = Schema.Struct({
                                   |  "title": Schema.String,
                                   |  "pages": Schema.Int,
                                   |  "read": Schema.Boolean
                                   |});""".stripMargin)
    ,
    /** A nullable field is written one way and read another, so there are two things to say and two names to say them
      * under.
      */
    test("an asymmetric schema is named twice"):
      assertTrue(module(note) == """export type NoteRead = Schema.Schema.Type<typeof NoteRead>;
                                   |
                                   |export const NoteRead = Schema.Struct({
                                   |  "title": Schema.String,
                                   |  "tag": Schema.optionalWith(Schema.Int, { "nullable": true })
                                   |});
                                   |
                                   |export type NoteWrite = Schema.Schema.Type<typeof NoteWrite>;
                                   |
                                   |export const NoteWrite = Schema.Struct({
                                   |  "title": Schema.String,
                                   |  "tag": Schema.NullOr(Schema.Int)
                                   |});""".stripMargin)
    ,
    /** Comparing a declaration to its counterpart once is not enough. `Outer` has the same shape on both sides -- one
      * key, holding `Note` -- and still cannot be named once, because the two `Note`s it would have to refer to are now
      * different names. Which is why the whole thing is rendered again until nothing new has to split.
      */
    test("a split propagates to whatever refers to it"):
      val outer = field("note", note).toRecord.attr(Keys.name, "Outer")

      assertTrue(module(outer) == """export type NoteRead = Schema.Schema.Type<typeof NoteRead>;
                                    |
                                    |export const NoteRead = Schema.Struct({
                                    |  "title": Schema.String,
                                    |  "tag": Schema.optionalWith(Schema.Int, { "nullable": true })
                                    |});
                                    |
                                    |export type OuterRead = Schema.Schema.Type<typeof OuterRead>;
                                    |
                                    |export const OuterRead = Schema.Struct({ "note": NoteRead });
                                    |
                                    |export type NoteWrite = Schema.Schema.Type<typeof NoteWrite>;
                                    |
                                    |export const NoteWrite = Schema.Struct({
                                    |  "title": Schema.String,
                                    |  "tag": Schema.NullOr(Schema.Int)
                                    |});
                                    |
                                    |export type OuterWrite = Schema.Schema.Type<typeof OuterWrite>;
                                    |
                                    |export const OuterWrite = Schema.Struct({ "note": NoteWrite });""".stripMargin)
    ,
    /** A symmetric definition under an asymmetric one is still shared, and is declared before either side needs it. */
    test("only what has to split does"):
      val name = (field("first", string) :* field("last", string)).attr(Keys.name, "Name")

      val outer = (field("author", name) :* field("tag", int).optional.nullable).attr(Keys.name, "Outer")

      assertTrue(module(outer) == """export type Name = Schema.Schema.Type<typeof Name>;
                                    |
                                    |export const Name = Schema.Struct({
                                    |  "first": Schema.String,
                                    |  "last": Schema.String
                                    |});
                                    |
                                    |export type OuterRead = Schema.Schema.Type<typeof OuterRead>;
                                    |
                                    |export const OuterRead = Schema.Struct({
                                    |  "author": Name,
                                    |  "tag": Schema.optionalWith(Schema.Int, { "nullable": true })
                                    |});
                                    |
                                    |export type OuterWrite = Schema.Schema.Type<typeof OuterWrite>;
                                    |
                                    |export const OuterWrite = Schema.Struct({
                                    |  "author": Name,
                                    |  "tag": Schema.NullOr(Schema.Int)
                                    |});""".stripMargin)
    ,
    /** Collapsing makes a name depend on whether the schema happens to be symmetric, so there is a policy that never
      * does.
      */
    test("suffixing splits whether it has to or not"):
      assertTrue(suffixed(book) == """export type BookRead = Schema.Schema.Type<typeof BookRead>;
                                     |
                                     |export const BookRead = Schema.Struct({
                                     |  "title": Schema.String,
                                     |  "pages": Schema.Int,
                                     |  "read": Schema.Boolean
                                     |});
                                     |
                                     |export type BookWrite = Schema.Schema.Type<typeof BookWrite>;
                                     |
                                     |export const BookWrite = Schema.Struct({
                                     |  "title": Schema.String,
                                     |  "pages": Schema.Int,
                                     |  "read": Schema.Boolean
                                     |});""".stripMargin)
    ,
    /** Several schemas share one run, so a definition two of them reach is declared once. */
    test("schemas rendered together share what they have in common"):
      val name = (field("first", string) :* field("last", string)).attr(Keys.name, "Name")
      val author = field("author", name).toRecord.attr(Keys.name, "Author")
      val editor = field("editor", name).toRecord.attr(Keys.name, "Editor")

      assertTrue(names(author, editor) == List("Name", "Author", "Editor"))
    ,
    /** A hoisted target constant is the same on any side, so it is never split and is declared once. */
    test("a coercion is shared, not split"):
      val schema = (field("count", coerce(int)) :* field("size", coerce(int))).attr(Keys.name, "Counts")

      assertTrue(names(schema) == List("CoerceNumber", "CountsRead", "CountsWrite"))
    ,
    /** Only a named schema has a declaration to contribute; an anonymous one has nowhere to go. */
    test("an anonymous schema contributes nothing to a module"):
      assertTrue(JsonTypescriptEffectRenderer.module(json.book).isEmpty)
  )
