package io.taig.otter.codec

import cats.syntax.all.*
import io.bullet.borer.Dom
import io.taig.otter.JsonBorerNumber
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

/** The number ladder, asserted absolutely, and the assumption underneath it.
  *
  * [[JsonBorerAgreementTest]] cannot secure this on its own: a ladder that is wrong the same way circe is wrong agrees
  * with it, and borer's own choice of element -- which is not this module's code, and not in its tests -- is what every
  * one of these answers depends on. So the first test here guards borer's parser and the rest state the answers.
  */
object JsonBorerNumberTest extends ZIOSpecDefault:
  /** Scala.js emulates `java.math.BigDecimal` and formats a `Float` by widening it to a `Double`, so a handful of
    * answers below are the platform's rather than this module's. `1.0f.toString` is `"1"` on the JVM and `"1"` on
    * Scala.js, but `0.1f.toString` is `"0.1"` and `"0.10000000149011612"`.
    */
  private val Jvm: Boolean = String.valueOf(0.1f) == "0.1"

  private def element(lexeme: String): Dom.Element = Doc.toBorer(Doc.Num(lexeme))

  private def number(lexeme: String): Option[JsonBorerNumber] = JsonBorerNumber.unapply(element(lexeme))

  private def double(lexeme: String): Boolean = element(lexeme) match
    case Dom.DoubleElem(_) => true
    case _                 => false

  private val lexemes: List[String] = DocTest.Lexemes ++ List(
    // Exponents stay under borer's own `maxNumberAbsExponent` of 64; over it the parser refuses the document
    // outright, which `JsonBorerDivergenceTest` asserts rather than this.
    "1e60",
    "1e-60",
    "0.5",
    "-1.25",
    "0.1234567890123456789",
    "12345678901234567890",
    "123456789012345678901234567890.5"
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerNumberTest")(
    /** The assumption everything else here rests on, in two halves. If either ever fails, borer has started rounding on
      * the way in, and no amount of care in [[JsonBorerNumber]] can recover a value the parser has thrown away.
      *
      * The first half is the guarantee: whichever element borer picks, the value survives. Note that it survives
      * through `LongElem` too, and `Long.MaxValue` is *not* exactly a `Double` -- which is why the guarantee is about
      * the value rather than about which element carries it.
      *
      * The second half is why there is no double rounding: a `DoubleElem` is chosen only when the decimal is exactly
      * double representable, so decimal to double to float cannot land anywhere decimal to float would not.
      */
    test("borer's parser never loses a number's value"):
      assertTrue(
        lexemes.forall: lexeme =>
          number(lexeme).flatMap(_.toBigDecimal).exists(_.compareTo(new JBigDecimal(lexeme)) == 0)
      )
    ,
    test("borer chooses a DoubleElem only for a value exactly double representable"):
      assertTrue(
        lexemes.forall: lexeme =>
          !double(lexeme) || new JBigDecimal(lexeme).compareTo(JBigDecimal.valueOf(lexeme.toDouble)) == 0
      )
    ,
    test("which element borer chooses, which is what the three cases are for"):
      assertTrue(
        number("412") == JsonBorerNumber.Integral(412).some,
        number("2147483648") == JsonBorerNumber.Integral(2147483648L).some,
        number("412.0") == JsonBorerNumber.Fractional(412.0).some,
        number("0.1234567890123456789") == JsonBorerNumber.Lexeme("0.1234567890123456789").some,
        JsonBorerNumber.unapply(Dom.StringElem("412")) == none,
        JsonBorerNumber.unapply(Dom.NullElem) == none,
        // A 65 bit CBOR integer is deliberately not a number here: reporting a type mismatch beats guessing.
        JsonBorerNumber.unapply(Dom.OverLongElem(negative = false, value = 1L)) == none
      )
    ,
    test("toInt is Some only when the value is exactly an Int, however it was written"):
      assertTrue(
        JsonBorerNumber.Integral(412).toInt == 412.some,
        JsonBorerNumber.Fractional(412.0).toInt == 412.some,
        JsonBorerNumber.Lexeme("4.12e2").toInt == 412.some,
        JsonBorerNumber.Lexeme("412.00").toInt == 412.some,
        JsonBorerNumber.Fractional(412.5).toInt == none,
        JsonBorerNumber.Integral(2147483648L).toInt == none,
        JsonBorerNumber.Fractional(-0.0).toInt == 0.some
      )
    ,
    test("toLong goes through BigInteger, because Double.toLong would saturate at 2^63 and accept it"):
      assertTrue(
        JsonBorerNumber.Lexeme("9223372036854775807").toLong == Long.MaxValue.some,
        JsonBorerNumber.Lexeme("-9223372036854775808").toLong == Long.MinValue.some,
        JsonBorerNumber.Lexeme("9223372036854775808").toLong == none,
        // 2^63 exactly, and exactly a Double, so this is the case a `toLong` would have silently accepted.
        JsonBorerNumber.Fractional(9.223372036854776e18).toLong == none,
        JsonBorerNumber.Fractional(412.5).toLong == none
      )
    ,
    test("toFloat and toDouble are total, as circe's are"):
      assertTrue(
        JsonBorerNumber.Integral(1).toFloat == 1.0f,
        JsonBorerNumber.Lexeme("0.1").toFloat == 0.1f,
        JsonBorerNumber.Lexeme("0.1").toDouble == 0.1,
        JsonBorerNumber.Lexeme("1e400").toDouble == Double.PositiveInfinity,
        JsonBorerNumber.Lexeme("1e-400").toDouble == 0.0,
        JsonBorerNumber.Fractional(Double.NaN).toDouble.isNaN
      )
    ,
    test("toBigInteger is integers only, and only under the digit cap circe uses"):
      assertTrue(
        JsonBorerNumber.Integral(412).toBigInteger == JBigInteger.valueOf(412).some,
        JsonBorerNumber.Fractional(412.0).toBigInteger == JBigInteger.valueOf(412).some,
        JsonBorerNumber.Fractional(412.5).toBigInteger == none,
        JsonBorerNumber.Lexeme("1e100").toBigInteger == JBigInteger.TEN.pow(100).some,
        // Twelve characters asking for a 256MB integer, which is what the cap is there to refuse.
        JsonBorerNumber.Lexeme("1e2147483647").toBigInteger == none,
        JsonBorerNumber.Fractional(Double.NaN).toBigInteger == none
      )
    ,
    test("toBigDecimal keeps the lexeme's own scale, and only the lexeme's"):
      assertTrue(
        JsonBorerNumber.Lexeme("1.50").toBigDecimal.map(_.scale) == 2.some,
        // The documented loss: borer's parser has already turned `1.50` into a Double by the time this sees it.
        JsonBorerNumber.Fractional(1.50).toBigDecimal.map(_.scale) == 1.some,
        JsonBorerNumber.Integral(412).toBigDecimal.map(_.scale) == 0.some,
        JsonBorerNumber.Fractional(Double.PositiveInfinity).toBigDecimal == none
      )
    ,
    /** `java.math.BigDecimal` is emulated on Scala.js and every one of these answers goes through it, so a platform
      * difference here would be a decoder that reads a document differently on two platforms -- worse than a difference
      * between two libraries. Asserted rather than trusted.
      */
    test("the java.math behaviour every answer above rests on"):
      assertTrue(
        JBigDecimal.valueOf(0.1).toString == "0.1",
        JBigDecimal.valueOf(-0.0).compareTo(JBigDecimal.ZERO) == 0,
        JsonBorerNumber.Lexeme("1e2147483649").toBigDecimal == none,
        // Refused on both platforms, by two different mechanisms, which is why this is the assertion and the
        // `toBigDecimal` answer below is not: on the JVM the digit cap catches it, and the cap counts in `Long`
        // because in `Int` the subtraction would overflow to a negative and wave a 2^31 digit integer through.
        JsonBorerNumber.Lexeme("1e2147483648").toBigInteger == none,
        new JBigDecimal("1e100").precision.toLong - new JBigDecimal("1e100").scale.toLong == 101L
      )
    ,
    /** The one answer that is not the same on both platforms, and the reason it does not matter.
      *
      * `1e2147483648` has a scale of exactly `Int.MinValue`, which the JVM's `BigDecimal` accepts and Scala.js's
      * emulation refuses. Reaching it needs a hand built `NumberStringElem`: borer's parser caps an absolute exponent
      * at 64 and would never produce one. And both platforms refuse it as a `BigInteger` regardless, which is the
      * answer a schema actually asks for.
      */
    test("a scale at the very edge of Int is where the two platforms part, out of reach of any document"):
      val parses = JsonBorerNumber.Lexeme("1e2147483648").toBigDecimal.nonEmpty

      assertTrue(parses == JsonBorerNumberTest.Jvm)
    ,
    test("lexeme, which is what a coercion to text writes"):
      assertTrue(
        JsonBorerNumber.Integral(412).lexeme == "412",
        JsonBorerNumber.Lexeme("1.50").lexeme == "1.50",
        // The documented loss again, on the coercion surface this time.
        JsonBorerNumber.Fractional(1.50).lexeme == "1.5"
      )
  )
