package io.taig.otter.sample

import io.taig.otter.http.TypescriptIssue
import io.taig.otter.http.codec.TypescriptEffectPayload
import io.taig.otter.http.codec.TypescriptEndpointRenderer
import io.taig.otter.sample.api.api
import zio.Scope
import zio.test.*

/** The TypeScript the endpoints render as.
  *
  * What is generated is a *descriptor* and not a client, and that is the whole design rather than an omission. A
  * generated function that fetched and decoded before returning would never let its caller hold the response as the
  * serialisable thing it arrived as, and a cache that requires serialisable values cannot keep what a schema decoded
  * into a `Date`. So the module carries the builders that write a request, the schema for each body, and both the
  * decoded and the encoded type of every answer -- and when to decode is the caller's.
  *
  * The first test is the one that matters: nothing generated calls `fetch`.
  */
object LibraryTypescriptTest extends ZIOSpecDefault:
  private val module = TypescriptEndpointRenderer.client(TypescriptEffectPayload.json).render(api.all)

  private val source: String = module.render

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryTypescriptTest")(
    suite("what is generated")(
      test("calls nothing: no fetch, no await, no async"):
        assertTrue(!source.contains("fetch("), !source.contains("await "), !source.contains("async "))
      ,
      test("imports the one library its schemas are written in"):
        assertTrue(source.startsWith("""import { Schema } from "effect";"""))
      ,
      test("declares a descriptor per served endpoint, named by its operation id"):
        assertTrue(
          source.contains("export const deleteBook = {"),
          source.contains("export const borrowBook = {"),
          source.contains("export const listBooks = {")
        )
      ,
      test("a descriptor says the method and builds the path out of its input"):
        assertTrue(
          source.contains("\"method\": \"DELETE\""),
          source.contains("\"path\": (input: DeleteBookInput) =>")
        )
      ,
      test("every status a three branch endpoint answers under is keyed by its code"):
        assertTrue(
          source.contains("\"201\": { \"application/json\": Loan }"),
          source.contains("\"404\": { \"application/json\": Problem }"),
          source.contains("\"409\": { \"application/json\": Problem }")
        )
      ,
      test("an answer with no entity is an empty record rather than a missing key"):
        assertTrue(source.contains("\"204\": {}"))
    ),
    suite("schemas")(
      test("a refinement written in Scala reaches the generated schema as a refinement"):
        assertTrue(
          source.contains("Schema.minLength(1)"),
          source.contains("Schema.maxLength(200)"),
          source.contains("Schema.greaterThan(0)")
        )
      ,
      test("an enumeration becomes the set of literals it maps onto"):
        assertTrue(
          source.contains(
            """Schema.Literal("biography", "children", "fantasy", "history", "poetry", "romance", "thriller")"""
          )
        )
      ,
      test("a dictionary becomes a record keyed by string"):
        assertTrue(source.contains("Schema.Record({"))
      ,
      test("a schema shared by several endpoints is declared once"):
        assertTrue(source.split("export const Book = ").length == 2)
      ,
      test("a schema that refers to itself is suspended, which is how a value refers to itself in TypeScript"):
        assertTrue(source.contains("Schema.suspend(() => Category)"))
      ,
      test("both the decoded and the encoded type of an answer are named, so a caller may cache the second"):
        assertTrue(source.contains("Schema.Schema.Type<"), source.contains("Schema.Schema.Encoded<"))
    ),
    suite("what could not be said")(
      test("a multipart payload is reported by the endpoint it was written on"):
        assertTrue(module.issues.exists:
          case TypescriptIssue.Multipart(operation) => operation.contains("/books/{isbn}/cover")
          case _                                    => false)
      ,
      test("both streamed answers are reported, whatever alphabet their elements were written in"):
        val streamed = module.issues.collect { case issue: TypescriptIssue.Streamed => issue }

        assertTrue(streamed.length == 2)
      ,
      test("nothing else is unsaid, and a module still came back"):
        assertTrue(module.issues.length == 3, module.declarations.nonEmpty)
    )
  )
