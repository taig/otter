package io.taig.otter

import zio.Scope
import zio.test.*

object TypescriptIdentifierTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptIdentifierTest")(
    test("valid portable identifiers are preserved"):
      assertTrue(List("Book", "book_2", "$book", "_", "a0").forall(name => TypescriptIdentifier(name) == name))
    ,
    test("punctuation, Unicode, empty names and leading digits become portable identifiers"):
      assertTrue(
        TypescriptIdentifier("not-valid") == "not_valid",
        TypescriptIdentifier("a.b/c d") == "a_b_c_d",
        TypescriptIdentifier("9lives") == "_9lives",
        TypescriptIdentifier("") == "_",
        TypescriptIdentifier("café") == "caf_",
        TypescriptIdentifier("a\n\"b") == "a__b"
      )
  )
