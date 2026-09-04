package io.taig.otter

import cats.syntax.all.*
import io.bullet.borer.Dom

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

/** A JSON number, as the six questions `io.circe.JsonNumber` answers.
  *
  * circe's number is one value asked six questions; borer's is four representations, chosen by borer's parser:
  * `IntElem` and `LongElem` for a value that fits one, `DoubleElem` for a decimal that is exactly double representable,
  * and `NumberStringElem` -- the raw lexeme -- for everything else. Reading a schema's target off the representation
  * would make `412` and `412.0` different numbers, which is the most ordinary document there is, so this restores the
  * value over the representation and answers the same six questions under the same contracts: the integral ones are
  * `Some` only when the value is exactly representable, and [[toFloat]] and [[toDouble]] are total.
  *
  * What that buys is a guarantee worth stating, and `JsonBorerNumberTest` states it: **borer preserves every number's
  * value exactly.** It loses only a lexeme's *formatting*, and only where it chose a `DoubleElem`, which it does only
  * for a decimal that is exactly double representable -- so there is no double rounding, because the intermediate
  * `Double` is exact. `Long.MaxValue` is the case that shows why the guarantee is about the value and not about the
  * element: it is not exactly a `Double`, and it still arrives whole, as a `LongElem`.
  *
  * Two things it cannot preserve, both because borer's parser has already decided before this code runs:
  *
  *   - The **scale** of a non canonically written decimal. `1.50` comes back as `1.5`, and
  *     `java.math.BigDecimal.equals` is scale sensitive, so a `Primitive.Number.BigDecimal` and every [[Data]] that
  *     reaches the `BigDecimal` rung compare unequal to circe's answer while comparing equal under `compareTo`.
  *   - The **sign of an integral zero**. `-0` arrives as an `IntElem(0)`, so a schema reading `Double` gets `0.0` where
  *     circe gets `-0.0`. `-0.0` written with a fraction is preserved, because that arrives as a `DoubleElem`.
  */
enum JsonBorerNumber:
  case Integral(value: Long)
  case Fractional(value: Double)
  case Lexeme(value: String)

  /** `Some` only if the value is exactly an `Int`, which is `io.circe.JsonNumber.toInt`'s contract. `412.0` is. */
  def toInt: Option[Int] = this match
    case JsonBorerNumber.Integral(value) => if value.isValidInt then value.toInt.some else none
    case _                               => toLong.filter(_.isValidInt).map(_.toInt)

  /** `Some` only if the value is exactly a `Long`. Routed through `BigInteger` rather than `Double.toLong`, which
    * saturates at `Long.MaxValue` and would silently accept 2^63.
    */
  def toLong: Option[Long] = this match
    case JsonBorerNumber.Integral(value) => value.some
    case _                               => toBigInteger.filter(_.bitLength < 64).map(_.longValue)

  /** Total, as circe's is. A magnitude no `Float` holds comes back as an infinity. */
  def toFloat: Float = this match
    case JsonBorerNumber.Integral(value)   => value.toFloat
    case JsonBorerNumber.Fractional(value) => value.toFloat
    case JsonBorerNumber.Lexeme(value)     => decimal.fold(java.lang.Float.parseFloat(value))(_.floatValue)

  /** Total, as circe's is. */
  def toDouble: Double = this match
    case JsonBorerNumber.Integral(value)   => value.toDouble
    case JsonBorerNumber.Fractional(value) => value
    case JsonBorerNumber.Lexeme(value)     => decimal.fold(java.lang.Double.parseDouble(value))(_.doubleValue)

  /** `Some` only if the value is an integer, and only up to [[JsonBorerNumber.MaxDigits]] of them.
    *
    * The cap is circe's `bigIntegerMaxDigits`, and it is not a nicety: `1e2147483647` is twelve characters that would
    * otherwise ask for a 256MB integer.
    */
  def toBigInteger: Option[JBigInteger] = decimal
    .filter(value => value.precision.toLong - value.scale.toLong <= JsonBorerNumber.MaxDigits)
    .flatMap(value => Either.catchOnly[ArithmeticException](value.toBigIntegerExact).toOption)

  /** `Some` unless the lexeme's scale overflows an `Int`, which is circe's limit too. */
  def toBigDecimal: Option[JBigDecimal] = decimal

  /** The number as it would be written out.
    *
    * The one place a coercion to text cannot agree with circe: circe keeps a number's lexeme, so `1.50` coerces to
    * `"1.50"`, while borer has already turned it into a `Fractional` and it coerces to `"1.5"`.
    */
  def lexeme: String = this match
    case JsonBorerNumber.Integral(value)   => value.toString
    case JsonBorerNumber.Fractional(value) => value.toString
    case JsonBorerNumber.Lexeme(value)     => value

  private def decimal: Option[JBigDecimal] = this match
    case JsonBorerNumber.Integral(value)   => JBigDecimal.valueOf(value).some
    case JsonBorerNumber.Fractional(value) =>
      if value.isFinite then JBigDecimal.valueOf(value).some else none
    case JsonBorerNumber.Lexeme(value) =>
      Either.catchOnly[NumberFormatException](new JBigDecimal(value)).toOption

object JsonBorerNumber:
  /** circe's own `bigIntegerMaxDigits`, mirrored so that the two agree on which documents are refused. */
  val MaxDigits: Long = 1 << 18

  /** The numeric elements, as a pattern. `OverLongElem` is deliberately absent: it holds a 65 bit CBOR integer that no
    * JSON document can carry, and reporting it as a type mismatch is better than guessing at its value.
    */
  def unapply(element: Dom.Element): Option[JsonBorerNumber] = element match
    case Dom.IntElem(value)          => JsonBorerNumber.Integral(value.toLong).some
    case Dom.LongElem(value)         => JsonBorerNumber.Integral(value).some
    case Dom.DoubleElem(value)       => JsonBorerNumber.Fractional(value).some
    case Dom.FloatElem(value)        => JsonBorerNumber.Fractional(value.toDouble).some
    case Dom.Float16Elem(value)      => JsonBorerNumber.Fractional(value.toDouble).some
    case Dom.NumberStringElem(value) => JsonBorerNumber.Lexeme(value).some
    case _                           => none
