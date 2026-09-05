package io.taig.otter.http.codec

import io.taig.otter.Typescript
import io.taig.otter.http.Parameter
import io.taig.otter.http.component.HttpComponent.*
import io.taig.otter.http.fixture.http
import zio.Scope
import zio.test.*

/** What a parameter is, in TypeScript.
  *
  * Two claims that have to agree and are made apart: the *type* a caller holds, and the *text* that value goes on the
  * wire as. A renderer that said `number` and then handed the wire a number rather than its spelling would compile and
  * be wrong.
  */
object ParameterTypescriptRendererTest extends ZIOSpecDefault:
  private val value: Typescript.Expression = Typescript.Expression.Symbol("value")

  private def render(parameter: Parameter.Node[?, ?]): String = ParameterTypescriptRenderer.render(parameter).render

  private def text(parameter: Parameter.Node[?, ?]): String = ParameterTypescriptRenderer.text(parameter, value).render

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ParameterTypescriptRendererTest")(
    suite("type")(
      test("a primitive is the type it holds and not the text it becomes"):
        assertTrue(render(int) == "number", render(string) == "string", render(boolean) == "boolean")
      ,
      /** A parameter given more than once is a list of what it holds. What that looks like on the wire -- a repeated
        * name, or one joined by commas -- is the position's business and not the value's.
        */
      test("a repeated parameter is an array of its element"):
        assertTrue(render(collection.list(string)) == "ReadonlyArray<string>")
      ,
      /** The laxer spellings a coercion accepts are read and never written, so a caller has nothing to choose between.
        */
      test("a coercion is its canonical form"):
        assertTrue(render(coerce(boolean)) == "boolean")
    ),
    suite("text")(
      test("a string is already text and is left alone"):
        assertTrue(text(string) == "value")
      ,
      test("a number and a boolean are spelled"):
        assertTrue(text(int) == "String(value)", text(boolean) == "String(value)")
      ,
      /** Every parameter is text on the wire, and that is as true of one given many times as of one given once: handing
        * a caller an array of numbers where a scalar would have been converted would be the same claim made two
        * different ways.
        */
      test("a repeated parameter is spelled element by element"):
        assertTrue(
          ParameterTypescriptRenderer.isRepeated(collection.list(int)),
          text(collection.list(int)) == "value.map(String)",
          text(collection.list(string)) == "value"
        )
    ),
    suite("agreement")(
      /** The type and the text are two halves of one claim, and this is where they are held together: whether a
        * conversion is emitted at all is decided by whether the values are text already, and nothing else may decide
        * it.
        */
      test("a conversion is emitted for exactly the parameters that are not text"):
        assertTrue(
          ParameterTypescriptRenderer.isText(string),
          !ParameterTypescriptRenderer.isText(int),
          !ParameterTypescriptRenderer.isText(boolean),
          ParameterTypescriptRenderer.isText(collection.list(string))
        )
      ,
      test("every query parameter the fixture names renders a type"):
        val fields = io.taig.otter.http.Queries.fields(http.listing)

        assertTrue(
          fields.map(_.name).toList == List("page", "tags"),
          fields.map(field => render(field.schema.value)).toList == List("number", "ReadonlyArray<string>")
        )
    )
  )
