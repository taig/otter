package io.taig.otter.codec

import cats.data.Validated
import io.taig.data.Data
import io.taig.otter.Json
import io.taig.otter.Step
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.violations
import io.taig.validation.std
import zio.Scope
import zio.test.*

object JsonDynamicContract:
  def apply(interpreter: JsonInterpreter): Spec[TestEnvironment & Scope, Any] =
    def decode[A](schema: Json.Reader[A], document: String): Validated[io.taig.otter.Violations, A] =
      interpreter.decode(schema, document)

    suite("dynamic JSON")(
      test("any reads and writes every JSON kind and numbers exact in the interpreter model"):
        val schema = dynamic.any
        val value = Data.Object(
          List(
            "null" -> Data.Null,
            "boolean" -> true,
            "string" -> "text",
            "array" -> Data.Array(List(1, Data.Null)),
            "integer" -> 9007199254740991L,
            "decimal" -> new java.math.BigDecimal("0.1")
          )
        )
        val document = interpreter.encode(schema, value)

        assertTrue(
          document.contains("9007199254740991"),
          document.contains("0.1"),
          decode(schema, document) == Validated.valid(value),
          decode(schema, "null") == Validated.valid(Data.Null),
          decode(schema, "0.1") == Validated.valid(new java.math.BigDecimal("0.1")),
          decode(schema, "9007199254740991") == Validated.valid(9007199254740991L)
        )
      ,
      test("object and array are typed, preserve empty values, and validate object counts"):
        val minimum = std.obj.minimum[List[(String, Data)]](1).contramap[Data.Object[Data]](_.values)
        val obj = dynamic.obj(minimum)
        val array = dynamic.array

        assertTrue(
          decode(obj, "{} ").isInvalid,
          decode(obj, "{\"x\":null}") == Validated.valid(Data.Object(List("x" -> Data.Null))),
          decode(array, "[]") == Validated.valid(Data.Array(List.empty)),
          decode(array, "{\"x\":1}").isInvalid,
          interpreter.encode(obj, Data.Object(List.empty)) == "{}",
          interpreter.encode(array, Data.Array(List.empty)) == "[]"
        )
      ,
      test("wrong kinds and invalid nested numbers retain their document paths"):
        val schema = field("payload", dynamic.array).toRecord
        val invalid = decode(schema, "{\"payload\":{\"items\":[]}}")

        assertTrue(
          decode(dynamic.obj, "[]").fold(violations.paths, _ => Nil) == List(Nil),
          decode(dynamic.array, "{}").fold(violations.paths, _ => Nil) == List(Nil),
          invalid.fold(violations.paths, _ => Nil) == List(List(Step.Field("payload")))
        )
      ,
      test("nullable wrappers consume null while missing-only optional preserves it"):
        val missingOnly = field("x", dynamic.any).optional.toRecord
        val nullable = field("x", dynamic.any).nullable.toRecord
        val optionalOrNull = field("x", dynamic.any).optionalOrNull.toRecord
        val nullableOrMissing = field("x", dynamic.any).nullableOrMissing.toRecord
        val payloadNullable = dynamic.any.nullable

        assertTrue(
          decode(missingOnly, "{}") == Validated.valid(None),
          decode(missingOnly, "{\"x\":null}") == Validated.valid(Some(Data.Null)),
          decode(nullable, "{}").isInvalid,
          decode(nullable, "{\"x\":null}") == Validated.valid(None),
          decode(optionalOrNull, "{\"x\":null}") == Validated.valid(None),
          decode(nullableOrMissing, "{\"x\":null}") == Validated.valid(None),
          decode(payloadNullable, "null") == Validated.valid(None),
          interpreter.encode(payloadNullable, Some(Data.Null)) == "null",
          interpreter.encode(missingOnly, Some(Data.Null)) == "{\"x\":null}"
        )
    )
