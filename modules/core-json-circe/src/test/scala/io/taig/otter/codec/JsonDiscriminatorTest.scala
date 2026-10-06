package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.JsonDiscriminator
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks
import scala.util.Try

object JsonDiscriminatorTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonDiscriminatorTest")(
    test("merged branches require records in the type system"):
      assertTrue(
        typeChecks("""branch.merged("a", field("x", int).toRecord)"""),
        !typeChecks("""branch.merged("a", int)"""),
        !typeChecks("""branch.merged("a", collection.list(int))""")
      )
    ,
    test("collisions fail when a merged branch is constructed, including converted records"):
      val record = (field("x", int) :* field("kind", string)).dimap[(Int, String), (Int, String)](identity)(identity)
      assertTrue(
        Try(branch.merged("a", field("type", string).toRecord)).isFailure,
        Try(branch.merged("a", record, JsonDiscriminator.Merged("kind"))).isFailure,
        Try(JsonDiscriminator.Nested("tag", "tag")).isFailure
      )
    ,
    test("mixed tagging, different keys and duplicate tags fail when a union is constructed"):
      assertTrue(
        Try(branch.nested("a", int) :+ branch("b", string)).isFailure,
        Try(branch("a", int) :+ branch.nested("b", string)).isFailure,
        Try(branch.nested("a", int) :+ branch.nested("a", string)).isFailure,
        Try(branch.nested("a", int) :+ branch.nested("b", string, JsonDiscriminator.Nested("kind"))).isFailure
      )
    ,
    test("nested and merged branches may share one discriminator"):
      val schema = branch.nested("a", int) :+ branch.merged("b", field("text", string).toRecord)
      assertTrue(
        JsonCirceInterpreter.roundTrip(schema, Left(1)) == Left(1).valid,
        JsonCirceInterpreter.roundTrip(schema, Right("hello")) == Right("hello").valid
      )
    ,
    test("tagged conversions preserve read and write directions"):
      assertTrue(
        typeChecks("""val schema: Json.Union.Reader[Either[Int, String]] =
          branch.nested("a", int.map(_ + 1)) :+ branch.nested("b", string.map(_.trim))"""),
        typeChecks("""val schema: Json.Union.Writer[Either[Int, String]] =
          branch.nested("a", int.contramap[Int](_ + 1)) :+ branch.nested("b", string.contramap[String](_.trim))""")
      )
  )
