package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.validation.Validation
import zio.Scope
import zio.test.*

/** A value written as the literal that denotes it.
  *
  * The floats are the point. `new BigDecimal(double)` is the exact constructor and spells the binary value in full, so
  * a literal built with it denotes a number no JSON encoder writes -- and a generated schema carrying one rejects the
  * document this library produced from the very same schema. `1.5` is exactly representable and says nothing about
  * that, which is why every case below that matters is a value that is not.
  */
object PrimitiveTypescriptExpressionLiteralEncoderTest extends ZIOSpecDefault:
  private def render[W](schema: Primitive[W, Any], value: W): String =
    PrimitiveTypescriptExpressionLiteralEncoder.encode(schema, value).render

  private val double: Primitive[Double, Double] = Primitive.Number.Double(Validation.valid)
  private val float: Primitive[Float, Float] = Primitive.Number.Float(Validation.valid)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("PrimitiveTypescriptExpressionLiteralEncoderTest")(
    test("a double that is not exactly representable is spelled the way it was written"):
      assertTrue(
        render(double, 0.1d) == "0.1",
        render(double, 3.14d) == "3.14",
        render(double, -0.3d) == "-0.3"
      )
    ,
    /** Scala.js has no float formatting of its own, so `0.1f` reads back as the double there. Either spelling is the
      * one the document carries on that platform, and the exact expansion is neither.
      */
    test("a float that is not exactly representable is spelled the way the platform spells it"):
      assertTrue(render(float, 0.1f) == 0.1f.toString, render(float, 0.1f).length < 20)
    ,
    test("a value that is exactly representable is unchanged"):
      assertTrue(render(double, 1.5d) == "1.5", render(float, 1.5f) == "1.5")
    ,
    /** A whole double keeps the fraction its own text carries, which is the one a JSON document carries too: circe
      * writes `2.0` for it. TypeScript reads `2.0` and `2` as the same number, so the literal denotes what it should
      * either way, and agreeing with the document is what matters. An integral primitive has no fraction to keep.
      */
    test("a whole number is spelled as its own type spells it"):
      assertTrue(
        render(double, 2d) == "2.0",
        render(Primitive.Number.Int(Validation.valid), 2) == "2",
        render(Primitive.Number.Long(Validation.valid), 2L) == "2"
      )
    ,
    test("text and booleans are unaffected"):
      assertTrue(
        render(Primitive.Text.Root(Validation.valid), "otter") == "\"otter\"",
        render(Primitive.Boolean.Root, true) == "true"
      )
  )
