package io.taig.otter.codec

import io.taig.otter.fixture
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*
import zio.test.ZIOSpecDefault
import io.taig.otter.Keys
import io.taig.otter.Json

object JsonTypescriptEffectRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptEffectRendererTest")(
    test("Json.Constant"):
      val schema = constant(string, "foobar")
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Literal("foobar")"""
      assertTrue(obtained == expected)
    ,
    test("Json.Collection"):
      val schema = collection.list(int)
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Array(Schema.Number)"""
      assertTrue(obtained == expected)
    ,
    test("Json.Dictionary"):
      val schema = dictionary.list(boolean)
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Record({
                       |  "key": Schema.String,
                       |  "value": Schema.Boolean
                       |})""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("Json.Enumeration"):
      val obtained = JsonTypescriptEffectRenderer.render(fixture.json.animal).mkString("\n\n")
      val expected = """Schema.Literal(
                       |  "bird",
                       |  "cat",
                       |  "dog"
                       |)""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("Json.Optional"):
      val schema = string.optional
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.NullOr(Schema.String)"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: boolean"):
      val obtained = JsonTypescriptEffectRenderer.render(boolean).mkString("\n\n")
      val expected = """Schema.Boolean"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: coerce boolean"):
      val obtained = JsonTypescriptEffectRenderer.render(coerce(boolean)).mkString("\n\n")
      val expected = """CoerceBoolean"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: number"):
      val obtained = JsonTypescriptEffectRenderer.render(float).mkString("\n\n")
      val expected = """Schema.Number"""
      assertTrue(obtained == expected)
    ,
    test("Json.Primitive: text"):
      val obtained = JsonTypescriptEffectRenderer.render(string).mkString("\n\n")
      val expected = """Schema.String"""
      assertTrue(obtained == expected)
    ,
    test("Json.Record"):
      val schema = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown")) :*
        field("pet", fixture.json.animal.optional)

      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")

      val expected = """Schema.Struct({
                       |  "name": Schema.String,
                       |  "age": Schema.optional(Schema.Number),
                       |  "gender": Schema.Literal("unknown"),
                       |  "pet": Schema.NullOr(
                       |    Schema.Literal(
                       |      "bird",
                       |      "cat",
                       |      "dog"
                       |    )
                       |  )
                       |})""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("Json.Tuple"):
      val schema = string :* int :* fixture.json.animal.optional
      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Tuple(
                       |  Schema.String,
                       |  Schema.Number,
                       |  Schema.NullOr(
                       |    Schema.Literal(
                       |      "bird",
                       |      "cat",
                       |      "dog"
                       |    )
                       |  )
                       |)""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("Union"):
      val schema = branch("foo", string) :+
        branch("bar", int) :+
        branch("foobar", fixture.json.animal)

      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")
      val expected = """Schema.Union(
                       |  Schema.String,
                       |  Schema.Number,
                       |  Schema.Literal(
                       |    "bird",
                       |    "cat",
                       |    "dog"
                       |  )
                       |)""".stripMargin
      assertTrue(obtained == expected)
    ,
    test("name"):
      val name = (field("first", string) :* field("last", string)).attr(Keys.name, "Name")

      val schema = field("name", name) :* field("age", int).optional

      val obtained = JsonTypescriptEffectRenderer.render(schema).mkString("\n\n")

      val expected = """type Name = Schema.Schema.Type<typeof Name>;
                       |
                       |const Name = Schema.Struct({
                       |  "first": Schema.String,
                       |  "last": Schema.String
                       |});
                       |
                       |Schema.Struct({
                       |  "name": Name,
                       |  "age": Schema.optional(Schema.Number)
                       |})""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("recursion"):
      lazy val student: Json.Record[?] = (
        field("name", string) :*
          field("courses", collection.list(course))
      ).attr(Keys.name, "Student")

      lazy val course = (
        field("title", string) :*
          field("members", collection.list(student))
      ).attr(Keys.name, "Course")

      val obtained = JsonTypescriptEffectRenderer.render(course).mkString("\n\n")

      val expected = """type Student = {
                       |  "name": string;
                       |  "courses": ReadonlyArray<Course>;
                       |};
                       |
                       |const Student: Schema.Schema<Student> = Schema.Struct({
                       |  "name": Schema.String,
                       |  "courses": Schema.Array(Schema.suspend(() => Course))
                       |});
                       |
                       |type Course = Schema.Schema.Type<typeof Course>;
                       |
                       |const Course = Schema.Struct({
                       |  "title": Schema.String,
                       |  "members": Schema.Array(Student)
                       |});
                       |
                       |Course""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("recursion: self"):
      lazy val person: Json.Record[?] = (
        field("name", string) :*
          field("mother", person).optional
      ).attr(Keys.name, "Person")

      val obtained = JsonTypescriptEffectRenderer.render(person).mkString("\n\n")

      val expected = """type Person = {
                       |  "name": string;
                       |  "mother"?: Person | undefined;
                       |};
                       |
                       |const Person: Schema.Schema<Person> = Schema.Struct({
                       |  "name": Schema.String,
                       |  "mother": Schema.optional(Schema.suspend(() => Person))
                       |});
                       |
                       |Person""".stripMargin

      assertTrue(obtained == expected)
  )
