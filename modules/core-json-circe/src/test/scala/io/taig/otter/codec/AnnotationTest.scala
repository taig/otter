package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

/** What a node still says about itself after an operator has built something out of it.
  *
  * `zip` and `alt` are where a schema is combined with another, and both used to start the result from an empty
  * [[io.taig.otter.Metadata]]. That is every product and sum operator, and the attribute it dropped most often was
  * [[Keys.name]] -- which is what makes a schema a `$defs` entry in a JSON Schema and a `lazy val` in a generated
  * TypeScript module, so a named record put beside anything quietly stopped being either.
  *
  * It stayed hidden because a name is almost always attached last: `(field :* field).attr(name, …)` is the shape every
  * call site uses, and the operators have already run by then. These put the name on first.
  */
object AnnotationTest extends ZIOSpecDefault:
  private val named: Json.Record[(String, String)] =
    (field("first", string) :* field("last", string)).attr(Keys.name, "Name")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("AnnotationTest")(
    test("a named record keeps its name when a field is appended"):
      assertTrue((named :* field("tag", int)).attr(Keys.name).contains("Name"))
    ,
    test("a named record keeps its name when a field is prepended"):
      assertTrue((field("tag", int) *: named).attr(Keys.name).contains("Name"))
    ,
    test("a named record keeps its name when another record is concatenated onto it"):
      assertTrue((named ++ json.book).attr(Keys.name).contains("Name"))
    ,
    /** The operand contributes what the receiver does not say, which is what makes this a combine rather than a choice
      * of one side.
      */
    test("an attribute only the operand carries is carried through"):
      val described = json.book.attr(Keys.description, "A book")

      assertTrue((named ++ described).attr(Keys.description).contains("A book"))
    ,
    test("a named union keeps its name when a branch is added"):
      val union = (branch("circle", json.circle) :+ branch("square", json.square)).attr(Keys.name, "Shape")

      assertTrue((union :+ branch("triangle", json.triangle)).attr(Keys.name).contains("Shape"))
    ,
    /** The regression this guards is not about metadata as such but about what a renderer then writes, so one of them
      * asks the renderer.
      */
    test("a name that survives an append is what the schema is declared under"):
      val schema = named :* field("tag", int)

      assertTrue(schema.attr(Keys.name).contains("Name"), named.attr(Keys.name).contains("Name"))
  )
