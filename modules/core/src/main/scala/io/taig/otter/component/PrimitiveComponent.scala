package io.taig.otter.component

import cats.arrow.Profunctor
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.net.URI
import java.net.URISyntaxException
import java.nio.charset.Charset
import java.util.Currency
import java.util.Locale
import java.util.UUID
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import scala.Boolean as SBoolean

object PrimitiveComponent:
  trait Boolean[F[-_, +_]](using F: PrimitiveOperation.Boolean[F]):
    val boolean: F[SBoolean, SBoolean] = F.boolean

  trait Number[F[-_, +_]](using F: PrimitiveOperation.Number[F]):
    def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal, JBigDecimal] =
      F.bigDecimal(validation)

    val jBigDecimal: F[JBigDecimal, JBigDecimal] = jBigDecimal(Validation.valid)

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, BigDecimal])(using
        P: Profunctor[F]
    ): F[BigDecimal, BigDecimal] =
      P.dimap(jBigDecimal(validation.contramap(BigDecimal.apply)))((_: BigDecimal).bigDecimal)(BigDecimal.apply)

    def bigDecimal(using Profunctor[F]): F[BigDecimal, BigDecimal] = bigDecimal(Validation.valid)

    def jBigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger, JBigInteger] =
      F.bigInteger(validation)

    val jBigInteger: F[JBigInteger, JBigInteger] = jBigInteger(Validation.valid)

    def bigInt(validation: Validation[Constraint.Primitive.Number, BigInt])(using
        P: Profunctor[F]
    ): F[BigInt, BigInt] =
      P.dimap(jBigInteger(validation.contramap(BigInt.apply)))((_: BigInt).bigInteger)(BigInt.apply)

    def bigInt(using Profunctor[F]): F[BigInt, BigInt] = bigInt(Validation.valid)

    def double(validation: Validation[Constraint.Primitive.Number, Double]): F[Double, Double] = F.double(validation)

    val double: F[Double, Double] = double(Validation.valid)

    def float(validation: Validation[Constraint.Primitive.Number, Float]): F[Float, Float] = F.float(validation)

    val float: F[Float, Float] = float(Validation.valid)

    def int(validation: Validation[Constraint.Primitive.Number, Int]): F[Int, Int] = F.int(validation)

    val int: F[Int, Int] = int(Validation.valid)

    def long(validation: Validation[Constraint.Primitive.Number, Long]): F[Long, Long] = F.long(validation)

    val long: F[Long, Long] = long(Validation.valid)

  trait Text[F[-_, +_]](using F: PrimitiveOperation.Text[F]):
    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String, String] = F.string(validation)

    val string: F[String, String] = string(Validation.valid)

    def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A, A] =
      F.format(name, parse, print)

    /** Text parsed on the way in and written back verbatim on the way out.
      *
      * The write side is the wire text itself, which is what the node genuinely does: `print` is `identity[String]`, so
      * encoding a `String` yields that `String`. It is not the read side's inverse, and a schema built on this does not
      * round trip -- ascribe it as a reader where only the read side is meant to be reachable.
      */
    def parser[A](name: String, parse: String => Either[String, A]): F[String, A] =
      F.format(name, parse, identity[String])

    /** Text brought into a normal form on the way in and written back verbatim on the way out.
      *
      * [[parser]] at `String` with a parse that cannot fail. `decode(encode(a))` is `a` for every value already in
      * normal form, which is every value that leaves a decoder, so this composes into a record that keeps both
      * directions. `encode(decode(document))` is not `document`, which is the point: a test writes the raw text and the
      * read side normalises it.
      *
      * Prefer this to `string.map(f)`, which describes the same wire behaviour and discards the write side. Spell the
      * result out as `Text.Schema[String, String]` rather than the round tripping `Text[String]` alias: the two
      * `String`s are not the same `String`.
      */
    def normalized(name: String, f: String => String): F[String, String] = parser(name, f(_).asRight)

    /** A schema that can only be written. */
    def printer[A](name: String, print: A => String): F[A, Any] =
      F.format(name, _ => Left(s"$name is write only"), print)

    val uuid: F[UUID, UUID] = codec(
      "uuid",
      value => Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_ => s"invalid uuid: $value"),
      _.toString
    )

    /** A locale as the BCP 47 language tag it is written as everywhere else in the API.
      *
      * `Locale.forLanguageTag` rather than `Locale.Builder.setLanguageTag`, which Scala.js has no implementation of.
      * The factory never rejects anything: it keeps the longest prefix of the text that is a language tag and drops the
      * rest, so the tag it prints back is what says whether it understood all of the input. Case is the one difference
      * it is allowed to make -- `"en-us"` reads back out as `"en-US"`, the way [[charset]] canonicalises -- and
      * anything the factory dropped, a `_` separator or a trailing subtag that is not one, leaves a shorter tag than
      * went in and reads as invalid.
      */
    lazy val locale: F[Locale, Locale] = codec(
      "locale",
      value =>
        Locale
          .forLanguageTag(value)
          .some
          .filter(_.toLanguageTag.equalsIgnoreCase(value))
          .toRight(s"invalid locale: $value"),
      _.toLanguageTag
    )

    /** An ISO 4217 currency code, the same shape as [[locale]]: a factory that rejects anything not in its registry,
      * and a carrier whose own `toString` is that registry's code.
      *
      * `catchNonFatal` rather than the `IllegalArgumentException` the JVM's factory throws, because the registry is not
      * the same registry on both platforms: Scala.js has no `Currency` of its own, and the scala-java-locales one is a
      * lookup in whichever CLDR currency data the application registered, which fails with `NoSuchElementException`.
      */
    lazy val currency: F[Currency, Currency] = codec(
      "currency",
      value => Either.catchNonFatal(Currency.getInstance(value)).leftMap(_ => s"invalid currency: $value"),
      _.getCurrencyCode
    )

    /** A `URI`, not a `URL`: `URL`'s `equals` and `hashCode` resolve hostnames over the network, which a schema must
      * never do. `URI` carries the same text without reaching out.
      */
    val uri: F[URI, URI] = codec(
      "uri",
      value => Either.catchOnly[URISyntaxException](new URI(value)).leftMap(_ => s"invalid uri: $value"),
      _.toString
    )

    /** A charset name or alias, canonicalised the way [[locale]] is: `"utf8"` reads back out as `"UTF-8"`.
      *
      * `Charset.forName` throws `IllegalCharsetNameException` for text no charset could ever be named and
      * `UnsupportedCharsetException` for a name this JVM does not have registered; both are `IllegalArgumentException`,
      * so one catch covers either.
      */
    val charset: F[Charset, Charset] = codec(
      "charset",
      value =>
        Either.catchOnly[IllegalArgumentException](Charset.forName(value)).leftMap(_ => s"invalid charset: $value"),
      _.name
    )

    /** A regular expression, carried as the source text `Pattern.compile` accepts and `.pattern` hands back unchanged.
      * Named `regex` rather than `pattern` so the value and the `java.util.regex.Pattern` it is built from never share
      * a name.
      */
    val regex: F[Pattern, Pattern] = codec(
      "regex",
      value => Either.catchOnly[PatternSyntaxException](Pattern.compile(value)).leftMap(_ => s"invalid regex: $value"),
      _.pattern
    )
