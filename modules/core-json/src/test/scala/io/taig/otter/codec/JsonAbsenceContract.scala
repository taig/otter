package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Json
import io.taig.otter.Step
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.violations
import zio.Scope
import zio.test.*

/** Absolute wire examples shared by every JSON interpreter, including all four field contracts. */
object JsonAbsenceContract:
  def apply(interpreter: JsonInterpreter): Spec[TestEnvironment & Scope, Any] =
    val optionals: List[(String, Json.Field[Option[Int]], Boolean, Boolean, String)] = List(
      ("optional", field("x", int).optional, true, false, "{}"),
      ("nullable", field("x", int).nullable, false, true, """{"x":null}"""),
      ("optionalOrNull", field("x", int).optionalOrNull, true, true, "{}"),
      ("nullableOrMissing", field("x", int).nullableOrMissing, true, true, """{"x":null}""")
    )
    val defaults: List[(String, Json.Field[Int], Boolean, Boolean)] = List(
      ("defaulted", field("x", int).defaulted(7), true, false),
      ("defaultedOnNull", field("x", int).defaultedOnNull(7), false, true),
      ("defaultedOnMissingOrNull", field("x", int).defaultedOnMissingOrNull(7), true, true)
    )
    suite("absence contracts")(
      suite("optional fields")(
        optionals.map { (name, field, missing, nulled, written) =>
          test(name):
            val schema = field.toRecord
            val absent = interpreter.decode(schema, "{}")
            val empty = interpreter.decode(schema, """{"x":null}""")
            val invalid = interpreter.decode(schema, """{"x":"bad"}""")
            assertTrue(
              absent.isValid == missing,
              !missing || absent == Validated.valid(None),
              empty.isValid == nulled,
              !nulled || empty == Validated.valid(None),
              interpreter.decode(schema, """{"x":3}""") == Validated.valid(Some(3)),
              invalid.fold(violations.paths, _ => Nil) == List(List(Step.Field("x"))),
              interpreter.encode(schema, None) == written,
              interpreter.encode(schema, Some(3)) == """{"x":3}"""
            )
        }
      ),
      suite("defaults")(
        defaults.map { (name, field, missing, nulled) =>
          test(name):
            val schema = field.toRecord
            val absent = interpreter.decode(schema, "{}")
            val empty = interpreter.decode(schema, """{"x":null}""")
            assertTrue(
              absent.isValid == missing,
              !missing || absent == Validated.valid(7),
              empty.isValid == nulled,
              !nulled || empty == Validated.valid(7),
              interpreter.decode(schema, """{"x":3}""") == Validated.valid(3),
              interpreter.decode(schema, """{"x":"bad"}""").isInvalid,
              interpreter.encode(schema, 7) == """{"x":7}""",
              interpreter.encode(schema, 3) == """{"x":3}"""
            )
        }
      ),
      test("PATCH preserves missing, null and present independently"):
        val schema = field("summary", string.nullable).optional.toRecord
        assertTrue(
          interpreter.decode(schema, "{}") == Validated.valid(None),
          interpreter.decode(schema, """{"summary":null}""") == Validated.valid(Some(None)),
          interpreter.decode(schema, """{"summary":"new"}""") == Validated.valid(Some(Some("new"))),
          interpreter.encode(schema, None) == "{}",
          interpreter.encode(schema, Some(None)) == """{"summary":null}""",
          interpreter.encode(schema, Some(Some("new"))) == """{"summary":"new"}"""
        )
      ,
      test("empty text is a present string, not JSON null"):
        val schema = field("x", string).optional.toRecord
        assertTrue(interpreter.decode(schema, """{"x":""}""") == Validated.valid(Some("")))
    )
