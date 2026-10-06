package io.taig.otter.codec

import cats.data.Validated
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Tagged
import io.taig.otter.fixture.TaggedTree
import io.taig.otter.fixture.violations
import io.taig.validation.Violation
import zio.Scope
import zio.test.*

import java.util.concurrent.atomic.AtomicInteger

object JsonDiscriminatorContract:
  def apply(interpreter: JsonInterpreter): Spec[TestEnvironment & Scope, Any] = suite("discriminators")(
    test("tagged recursion keeps payload references lazy"):
      val value = TaggedTree.Fork(List(TaggedTree.End, TaggedTree.Fork(List(TaggedTree.End))))
      assertTrue(
        interpreter.roundTrip(TaggedTree.schema, value) == Validated.valid(value),
        interpreter.roundTrip(TaggedTree.merged, value) == Validated.valid(value)
      )
    ,
    test("a known tag never invokes another branch's payload decoder"):
      val visited = new AtomicInteger(0)
      val other = parser[Int](
        "other",
        text =>
          visited.incrementAndGet()
          Right(text.length)
      )
      val schema = branch.nested("other", other) :+ branch.nested("selected", string)
      val result = interpreter.decode(schema, """{"type":"selected","value":"hello"}""")
      assertTrue(result == Validated.valid(Right("hello")), visited.get() == 0)
    ,
    test("nested and merged documents select their named enum case"):
      assertTrue(
        Tagged.nestedDocuments.forall((value, text) =>
          interpreter.decode(Tagged.nested, text) == Validated.valid(value)
        ),
        Tagged.mergedDocuments.forall((value, text) =>
          interpreter.decode(Tagged.merged, text) == Validated.valid(value)
        ),
        interpreter.decode(Tagged.custom, """{"kind":"none"}""") == Validated.valid(Right(()))
      )
    ,
    test("a missing tag is required at its wire path"):
      val expected = Validated.invalid(
        "type" /: Violations(Violation(Constraint.Generic.Required, Data.Null, None))
      )
      assertTrue(
        interpreter.decode(Tagged.nested, "{}") == expected,
        interpreter.decode(Tagged.merged, "{}") == expected
      )
    ,
    test("an unknown tag lists all allowed tags at its wire path"):
      val expected = Validated.invalid(
        "type" /: Violations(
          Violation(
            Constraint.Generic.OneOf(List("circle", "code", "numbers", "none").map(_.asData)),
            "other".asData,
            None
          )
        )
      )
      assertTrue(
        interpreter.decode(Tagged.nested, """{"type":"other"}""") == expected,
        interpreter.decode(Tagged.merged, """{"type":"other"}""") == expected
      )
    ,
    test("only the selected branch contributes violations"):
      val nested = interpreter.decode(Tagged.nested, """{"type":"circle","value":{"radius":"bad"}}""")
      val merged = interpreter.decode(Tagged.merged, """{"type":"circle","radius":"bad"}""")
      assertTrue(
        nested.fold(violations.paths, _ => Nil) == List(List(Step.Field("value"), Step.Field("radius"))),
        merged.fold(violations.paths, _ => Nil) == List(List(Step.Field("radius"))),
        nested.fold(violations.constraints, _ => Nil) == List(Constraint.Generic.Type("int")),
        merged.fold(violations.constraints, _ => Nil) == List(Constraint.Generic.Type("int"))
      )
    ,
    test("a payload is required except for unit and singleton branches"):
      val result = interpreter.decode(Tagged.nested, """{"type":"code"}""")
      assertTrue(
        result.fold(violations.paths, _ => Nil) == List(List(Step.Field("value"))),
        result.fold(violations.constraints, _ => Nil) == List(Constraint.Generic.Required)
      )
    ,
    test("non-object documents and non-string tags retain type diagnostics"):
      val document = interpreter.decode(Tagged.nested, "[]")
      val tag = interpreter.decode(Tagged.nested, """{"type":1}""")
      assertTrue(
        document.fold(violations.constraints, _ => Nil) == List(Constraint.Generic.Type("object")),
        tag.fold(violations.paths, _ => Nil) == List(List(Step.Field("type"))),
        tag.fold(violations.constraints, _ => Nil) == List(Constraint.Generic.Type("string"))
      )
  )
