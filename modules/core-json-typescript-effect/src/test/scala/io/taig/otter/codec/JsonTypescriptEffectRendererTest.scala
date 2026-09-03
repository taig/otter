package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect
import io.taig.otter.TypescriptKeys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Tree
import io.taig.otter.fixture.json
import io.taig.validation.Comparison
import io.taig.validation.std.collection as collections
import io.taig.validation.std.number
import io.taig.validation.std.text
import zio.Scope
import zio.test.*

import java.util.regex.Pattern

/** One node of the alphabet at a time, on the side where both agree. The two sides are
  * [[JsonTypescriptEffectDirectionTest]]; naming a schema and putting both sides in one module is
  * [[JsonTypescriptEffectModuleTest]].
  */
object JsonTypescriptEffectRendererTest extends ZIOSpecDefault:
  private def render(schema: Json.Node[?, ?]): String =
    JsonTypescriptEffectRenderer.writer.render(schema).mkString("\n\n")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptEffectRendererTest")(
    suite("primitive")(
      test("a boolean and a text"):
        assertTrue(render(boolean) == "Schema.Boolean", render(string) == "Schema.String")
      ,
      /** Which number node it is decides whether a fraction is admissible: the decoder of an `int` refuses one, so the
        * generated schema has to as well.
        */
      test("an integral number says so, and a fractional one does not"):
        assertTrue(
          render(int) == "Schema.Int",
          render(long) == "Schema.Int",
          render(jBigInteger) == "Schema.Int",
          render(double) == "Schema.Number",
          render(float) == "Schema.Number",
          render(jBigDecimal) == "Schema.Number"
        )
      ,
      /** Every text is a string on the wire whatever it parses into, and the four text constructors land in the same
        * node, so none of them narrows anything here. [[TypescriptKeys.expression]] is how a caller says otherwise.
        */
      test("a format is a string, whichever direction it was built for"):
        assertTrue(
          render(uuid) == "Schema.String",
          render(json.trimmed) == "Schema.String",
          render(json.title) == "Schema.String",
          render(json.isbn) == "Schema.String"
        )
    ),
    suite("structure")(
      test("a record"):
        assertTrue(render(json.book) == """Schema.Struct({
                                          |  "title": Schema.String,
                                          |  "pages": Schema.Int,
                                          |  "read": Schema.Boolean
                                          |})""".stripMargin)
      ,
      test("an empty record"):
        assertTrue(render(RNil) == "Schema.Struct({})")
      ,
      test("a collection"):
        assertTrue(render(collection.list(int)) == "Schema.Array(Schema.Int)")
      ,
      test("a dictionary names the key it does not otherwise describe"):
        assertTrue(render(dictionary.list(boolean)) == """Schema.Record({
                                                         |  "key": Schema.String,
                                                         |  "value": Schema.Boolean
                                                         |})""".stripMargin)
      ,
      test("a tuple"):
        assertTrue(
          render(
            TNil :* string :* int :* json.genre.optional
          ) == """Schema.Tuple(
                 |  Schema.String,
                 |  Schema.Int,
                 |  Schema.NullOr(Schema.Literal("fiction", "history", "poetry"))
                 |)""".stripMargin
        )
      ,
      /** A branch's name is only the label an error is reported under; the encoder never writes it and the decoder
        * tries the branches in order. So a union is untagged, and nothing here invents a discriminator.
        */
      test("a union is untagged"):
        val schema = branch("foo", string) :+ branch("bar", int) :+ branch("baz", json.genre)

        assertTrue(
          render(
            schema
          ) == """Schema.Union(Schema.String, Schema.Int, Schema.Literal("fiction", "history", "poetry"))""",
          render(json.shape) == """Schema.Union(
                                  |  Schema.Struct({ "radius": Schema.Number }),
                                  |  Schema.Struct({ "side": Schema.Number }),
                                  |  Schema.Struct({
                                  |    "base": Schema.Number,
                                  |    "height": Schema.Number
                                  |  })
                                  |)""".stripMargin
        )
      ,
      test("an optional schema is nullable, because that is what the codecs write for nothing"):
        assertTrue(render(string.optional) == "Schema.NullOr(Schema.String)")
    ),
    suite("literal")(
      /** The value is pushed back through the very schema that describes it, so a constant reads its own literal off
        * the primitive rather than off its Scala type.
        */
      test("a constant"):
        assertTrue(
          render(constant(string, "foobar")) == """Schema.Literal("foobar")""",
          render(constant(int, 42)) == "Schema.Literal(42)",
          render(constant(boolean, true)) == "Schema.Literal(true)"
        )
      ,
      test("an enumeration lists every value it admits"):
        assertTrue(render(json.genre) == """Schema.Literal("fiction", "history", "poetry")""")
    ),
    suite("constraint")(
      /** A `Validation` keeps its constraints, so what a primitive was built with is still readable and can be piped
        * onto the generated schema.
        */
      test("a text, a number and a collection carry their constraints"):
        val schema = field("title", string(text.minimum[String](3))) :*
          field("pages", int(number.minimum(Comparison(1, exclusive = false)))) :*
          field("tags", collection.list(string, collections.maximum[List[String]](5)))

        assertTrue(render(schema) == """Schema.Struct({
                                       |  "title": Schema.String.pipe(Schema.minLength(3)),
                                       |  "pages": Schema.Int.pipe(Schema.greaterThanOrEqualTo(1)),
                                       |  "tags": Schema.Array(Schema.String).pipe(Schema.maxItems(5))
                                       |})""".stripMargin)
      ,
      /** A bound on a length is a bound on an integer, so the exclusive form is the inclusive one next to it. */
      test("an exclusive bound on a length becomes the inclusive one beside it"):
        assertTrue(
          render(
            string(text.minimum[String](Comparison(3L, exclusive = true)))
          ) == "Schema.String.pipe(Schema.minLength(4))",
          render(
            string(text.maximum[String](Comparison(9L, exclusive = true)))
          ) == "Schema.String.pipe(Schema.maxLength(8))"
        )
      ,
      /** An exclusive bound on a number is a different filter, not a different reference. */
      test("an exclusive bound on a number is its own filter"):
        assertTrue(
          render(double(number.minimum(Comparison(1.5, exclusive = true)))) ==
            "Schema.Number.pipe(Schema.greaterThan(1.5))",
          render(double(number.maximum(Comparison(1.5, exclusive = false)))) ==
            "Schema.Number.pipe(Schema.lessThanOrEqualTo(1.5))"
        )
      ,
      /** effect has no filter for a unique collection, and rendering something else would claim more than the server
        * checks. A generated schema that validates less is safe; one that validates differently is not.
        */
      test("a constraint with no counterpart is left out rather than approximated"):
        val schema = collection.list(string, collections.uniqueItemsF[List, String])
        assertTrue(render(schema) == "Schema.Array(Schema.String)")
      ,
      /** effect has an array that says it holds something, and only that array types as one. A minimum of one is
        * therefore spent on the array rather than on a filter beside it, however it was spelled.
        */
      test("a collection that is never empty is the array effect has a name for"):
        assertTrue(
          render(collection.list(string, collections.minimum[List[String]](1))) ==
            "Schema.NonEmptyArray(Schema.String)",
          render(collection.list(string, collections.minimum[List[String]](Comparison(0L, exclusive = true)))) ==
            "Schema.NonEmptyArray(Schema.String)"
        )
      ,
      /** Only the minimum is spent; whatever else the collection was built with still has to be said. */
      test("a non empty collection keeps the constraints the array does not say"):
        val schema = collection.list(
          string,
          collections.minimum[List[String]](1).and(collections.maximum[List[String]](5))
        )

        assertTrue(render(schema) == "Schema.NonEmptyArray(Schema.String).pipe(Schema.maxItems(5))")
      ,
      /** A minimum of anything else is a bound, not a promise that there is a first element. */
      test("a collection with a larger minimum is still an array with a filter"):
        assertTrue(
          render(collection.list(string, collections.minimum[List[String]](2))) ==
            "Schema.Array(Schema.String).pipe(Schema.minItems(2))"
        )
    ),
    /** That a pattern reaches the filter at all, and that [[TypescriptRegex]] is what decides whether there is one.
      * Which patterns translate is asked of that directly, in `TypescriptRegexTest`: a `java.util.regex.Pattern` cannot
      * even be built out of the interesting ones on Scala.js.
      */
    suite("pattern")(
      test("a pattern JavaScript agrees with becomes a filter, under the unicode flag"):
        assertTrue(
          render(string(text.matches[String](Pattern.compile("^[a-z]+$")))) ==
            "Schema.String.pipe(Schema.pattern(/^[a-z]+$/u))"
        )
      ,
      /** `/` ends the literal, so a pattern that means one as a character has to say so. */
      test("a slash in a pattern is escaped rather than closing the literal"):
        assertTrue(
          render(string(text.matches[String](Pattern.compile("^a/b$")))) ==
            "Schema.String.pipe(Schema.pattern(/^a\\/b$/u))"
        )
    ),
    suite("name")(
      /** A name is the only thing that can make a declaration: `.to[Book]` is two closures by the time a renderer sees
        * it, so nothing else survives to be named.
        */
      test("a named schema is hoisted and referred to"):
        val person = (field("first", string) :* field("last", string)).attr(Keys.name, "Name")
        val schema = field("name", person) :* field("age", int).optional.omitted.strict

        assertTrue(render(schema) == """export type Name = Schema.Schema.Type<typeof Name>;
                                       |
                                       |export const Name = Schema.Struct({
                                       |  "first": Schema.String,
                                       |  "last": Schema.String
                                       |});
                                       |
                                       |Schema.Struct({
                                       |  "name": Name,
                                       |  "age": Schema.optional(Schema.Int)
                                       |})""".stripMargin)
      ,
      /** Reaching the same name twice is not recursion. It used to be read as one, because the name was left behind on
        * the stack after its own body was finished.
        */
      test("a name reached twice is declared once and suspended never"):
        val person = (field("first", string) :* field("last", string)).attr(Keys.name, "Name")
        val schema = field("author", person) :* field("editor", person)

        assertTrue(render(schema) == """export type Name = Schema.Schema.Type<typeof Name>;
                                       |
                                       |export const Name = Schema.Struct({
                                       |  "first": Schema.String,
                                       |  "last": Schema.String
                                       |});
                                       |
                                       |Schema.Struct({
                                       |  "author": Name,
                                       |  "editor": Name
                                       |})""".stripMargin)
    ),
    suite("recursion")(
      /** The name has to be on the schema that is reached again, which is the `lazy val` itself. Naming a wrapper
        * around it leaves the inner reference anonymous, and a renderer that never sees a name it has already seen
        * cannot stop.
        */
      test("a schema that refers to itself declares its own type and suspends the reference"):
        lazy val tree: Json.Record[Tree] =
          (field("value", int) :* field("children", collection.list(tree))).to[Tree].attr(Keys.name, "Tree")

        assertTrue(render(tree) == """export type Tree = {
                                     |  "value": number;
                                     |  "children": ReadonlyArray<Tree>;
                                     |};
                                     |
                                     |export const Tree: Schema.Schema<Tree> = Schema.Struct({
                                     |  "value": Schema.Int,
                                     |  "children": Schema.Array(Schema.suspend(() => Tree))
                                     |});
                                     |
                                     |Tree""".stripMargin)
      ,
      /** A declared type has to say what the value beside it says: a schema is invariant in the type it is ascribed, so
        * a structural type that widened a non empty array back to a plain one would not compile against its own value.
        */
      test("a recursive declaration says non empty in its type as well as in its value"):
        lazy val tree: Json.Record.Writer[Tree] = (
          field("value", int) :*
            field("children", collection.list(tree, collections.minimum[List[Any]](1)))
        ).attr(Keys.name, "Tree").contramap(tree => (tree.value, tree.children))

        assertTrue(
          render(tree).contains("\"children\": readonly [Tree, ...ReadonlyArray<Tree>];"),
          render(tree).contains("\"children\": Schema.NonEmptyArray(Schema.suspend(() => Tree))")
        )
      ,
      /** A cycle belongs to the definition that closes it and to no other. `Genre` is reached after the suspension and
        * has no cycle of its own, so it infers its type like any other name; it used to be told one, because the flag
        * the suspension set was still standing when its body was rendered.
        */
      test("a name after a cycle in the same body is not itself recursive"):
        val size = (field("height", int) :* field("width", int)).attr(Keys.name, "Size")

        lazy val tree: Json.Record.Writer[Tree] = (
          field("value", int) :*
            field("children", collection.list(tree)) :*
            field("size", size)
        ).attr(Keys.name, "Tree").contramap(tree => (tree.value, tree.children, (0, 0)))

        assertTrue(
          render(tree).contains("export type Size = Schema.Schema.Type<typeof Size>;"),
          !render(tree).contains("export const Size: Schema.Schema<Size>")
        )
      ,
      /** Only the declaration that holds the suspension cannot infer its type. `Student` is entered first and refers
        * forward, so it is the one told its shape; `Course` reaches an already declared `Student` and infers.
        */
      test("mutual recursion tells only the declaration that reaches forward"):
        lazy val student: Json.Record[JsonTypescriptEffectRendererTest.Student] =
          (field("name", string) :* field("courses", collection.list(course)))
            .to[JsonTypescriptEffectRendererTest.Student]
            .attr(Keys.name, "Student")

        lazy val course: Json.Record[JsonTypescriptEffectRendererTest.Course] =
          (field("title", string) :* field("members", collection.list(student)))
            .to[JsonTypescriptEffectRendererTest.Course]
            .attr(Keys.name, "Course")

        assertTrue(render(course) == """export type Student = {
                                       |  "name": string;
                                       |  "courses": ReadonlyArray<Course>;
                                       |};
                                       |
                                       |export const Student: Schema.Schema<Student> = Schema.Struct({
                                       |  "name": Schema.String,
                                       |  "courses": Schema.Array(Schema.suspend(() => Course))
                                       |});
                                       |
                                       |export type Course = Schema.Schema.Type<typeof Course>;
                                       |
                                       |export const Course = Schema.Struct({
                                       |  "title": Schema.String,
                                       |  "members": Schema.Array(Student)
                                       |});
                                       |
                                       |Course""".stripMargin)
    ),
    suite("override")(
      test("an expression a schema asks for is used instead of the derived one"):
        val schema = field("foo", string) :*
          field("bar", int.attr(TypescriptKeys.expression, TypescriptEffect.symbol("NumberFromString")))

        assertTrue(render(schema) == """Schema.Struct({
                                       |  "foo": Schema.String,
                                       |  "bar": Schema.NumberFromString
                                       |})""".stripMargin)
      ,
      test("an overridden expression is still hoisted when it is named"):
        val bar = int
          .attr(Keys.name, "Bar")
          .attr(TypescriptKeys.expression, TypescriptEffect.symbol("NumberFromString"))

        assertTrue(render(field("bar", bar).toRecord) == """export type Bar = Schema.Schema.Type<typeof Bar>;
                                                           |
                                                           |export const Bar = Schema.NumberFromString;
                                                           |
                                                           |Schema.Struct({ "bar": Bar })""".stripMargin)
      ,
      /** Declaring a type is what makes the constant need an ascription: inference would otherwise contradict what was
        * just declared.
        */
      test("a type a schema asks for wins over inference, and forces an ascription"):
        val bar = int.attr(Keys.name, "Bar").attr(TypescriptKeys.tpe, Typescript.Type.Symbol("unknown", Nil))

        assertTrue(render(field("bar", bar).toRecord) == """export type Bar = unknown;
                                                           |
                                                           |export const Bar: Schema.Schema<Bar> = Schema.Int;
                                                           |
                                                           |Schema.Struct({ "bar": Bar })""".stripMargin)
    )
  )

  final private case class Student(name: String, courses: List[JsonTypescriptEffectRendererTest.Course])

  final private case class Course(title: String, members: List[JsonTypescriptEffectRendererTest.Student])
