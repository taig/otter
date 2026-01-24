package io.taig.otter.codec

import io.taig.otter.Keys
import io.taig.otter.Json
import io.taig.otter.fixture
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*
import zio.test.ZIOSpecDefault
import cats.syntax.all.*

object JsonTypescriptZodRendererTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptZodRendererTest")(
    test("record"):
      val schema = field("name", string) :*
        field("age", int).optional :*
        field("gender", constant(string, "unknown")) :*
        field("pet", fixture.json.animal.optional)

      val obtained = JsonTypescriptZodRenderer.render(schema).mkString("\n\n")

      val expected = """z.object({
                       |  "name": z.string(),
                       |  "age": z.optional(z.number()),
                       |  "gender": z.literal("unknown"),
                       |  "pet": z.nullable(
                       |    z.enum(
                       |      [
                       |        "bird",
                       |        "cat",
                       |        "dog"
                       |      ]
                       |    )
                       |  )
                       |})""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("name"):
      val name = (field("first", string) :* field("last", string)).attr(Keys.name, "Name")

      val schema = field("name", name) :* field("age", int).optional

      val obtained = JsonTypescriptZodRenderer.render(schema).mkString("\n\n")

      val expected = """type Name = z.infer<typeof Name>;
                       |
                       |const Name = z.object({
                       |  "first": z.string(),
                       |  "last": z.string()
                       |});
                       |
                       |z.object({
                       |  "name": Name,
                       |  "age": z.optional(z.number())
                       |})""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("recursion: self"):
      lazy val person: Json.Record[?] = (
        field("name", string) :*
          field("mother", person).optional
      ).attr(Keys.name, "Person")

      val obtained = JsonTypescriptZodRenderer.render(person).mkString("\n\n")

      val expected = """type Person = {
                       |  "name": string;
                       |  "mother"?: Person | undefined;
                       |};
                       |
                       |const Person: z.ZodType<Person> = z.object({
                       |  "name": z.string(),
                       |  "mother": z.optional(z.lazy(() => Person))
                       |});
                       |
                       |Person""".stripMargin

      assertTrue(obtained == expected)
    ,
    test("recursion: self"):
      lazy val student: Json.Record[?] = (
        field("name", string) :*
          field("courses", collection.list(course))
      ).attr(Keys.name, "Student")

      lazy val course = (
        field("title", string) :*
          field("members", collection.list(student))
      ).attr(Keys.name, "Course")

      val obtained = JsonTypescriptZodRenderer.render(course).mkString("\n\n")

      val expected = """type Student = {
                       |  "name": string;
                       |  "courses": Array<Course>;
                       |};
                       |
                       |const Student: z.ZodType<Student> = z.object({
                       |  "name": z.string(),
                       |  "courses": z.array(z.lazy(() => Course))
                       |});
                       |
                       |type Course = z.infer<typeof Course>;
                       |
                       |const Course = z.object({
                       |  "title": z.string(),
                       |  "members": z.array(Student)
                       |});
                       |
                       |Course""".stripMargin

      assertTrue(obtained == expected)
  )
