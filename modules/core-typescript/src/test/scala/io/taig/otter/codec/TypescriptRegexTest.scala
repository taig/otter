package io.taig.otter.codec

import zio.Scope
import zio.test.*

/** A `java.util.regex` pattern read as a JavaScript one, asked of the text rather than of a compiled `Pattern`.
  *
  * The patterns that matter here are the ones the two flavours disagree about, and a `java.util.regex.Pattern` cannot
  * be built out of several of them on every platform this cross builds for: Scala.js refuses an embedded flag before
  * anything gets the chance to translate it. The translation is a function of the text either way, so that is what is
  * asked.
  */
object TypescriptRegexTest extends ZIOSpecDefault:
  private def render(pattern: String): Option[String] = TypescriptRegex(pattern).map(_.render)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptRegexTest")(
    suite("translated")(
      /** The flag is always set, because it is what makes JavaScript read `\p{…}` as the property Java meant. */
      test("a pattern both flavours read the same way carries the unicode flag"):
        assertTrue(
          render("^[a-z]+$").contains("/^[a-z]+$/u"),
          render("^[\\p{L}0-9._%+-]+@[\\p{L}0-9.-]+\\.[\\p{L}]{2,}$")
            .contains("/^[\\p{L}0-9._%+-]+@[\\p{L}0-9.-]+\\.[\\p{L}]{2,}$/u")
        )
      ,
      /** `/` ends the literal, so one that is meant as a character says so; one already escaped stays as it was. */
      test("a slash is escaped rather than closing the literal"):
        assertTrue(render("^a/b$").contains("/^a\\/b$/u"), render("^a\\/b$").contains("/^a\\/b$/u"))
      ,
      /** The constructs both flavours share, which is most of what a schema says. */
      test("groups, quantifiers, classes and backreferences pass through"):
        assertTrue(
          render("^(?:[0-9]+|(?=.*\\p{L})[\\p{L}0-9.-]+)$").isDefined,
          render("^[\\w](?!.*?\\.{2})[\\w.]*[\\w]$").isDefined,
          render("^(?<year>\\d{4})-\\k<year>$").isDefined,
          render("^\\d{2,4}$").isDefined
        )
      ,
      test("a property JavaScript also knows is kept"):
        assertTrue(render("\\p{Lu}").isDefined, render("\\p{Script=Greek}").isDefined, render("\\P{N}").isDefined)
    ),
    suite("refused")(
      /** An inline flag is a syntax error in a literal, and a module that will not parse is worse than a schema that
        * checks less than the server does.
        */
      test("an inline flag group"):
        assertTrue(render("^(?i)[A-Z]{2}$").isEmpty, render("(?s:a)").isEmpty, render("(?idmsux-idmsux)a").isEmpty)
      ,
      test("an atomic group and a possessive quantifier"):
        assertTrue(
          render("^(?>a)$").isEmpty,
          render("^a++$").isEmpty,
          render("^a*+$").isEmpty,
          render("^a?+$").isEmpty,
          render("^a{2,3}+$").isEmpty
        )
      ,
      /** `\h` is Java's horizontal whitespace and nothing at all in JavaScript; `\v` is a class to Java and the one
        * vertical tab to JavaScript, which is the quieter disagreement and the reason both are refused.
        */
      test("an escape JavaScript lacks or reads differently"):
        assertTrue(
          render("^\\h+$").isEmpty,
          render("^\\v+$").isEmpty,
          render("^\\R$").isEmpty,
          render("^\\A.\\z$").isEmpty,
          render("^\\Qa.b\\E$").isEmpty
        )
      ,
      /** Java's own property spellings, which a JavaScript engine does not know under `u`. */
      test("a property name only Java knows"):
        assertTrue(
          render("^\\p{Alpha}+$").isEmpty,
          render("^\\p{IsAlphabetic}+$").isEmpty,
          render("^\\p{javaLowerCase}+$").isEmpty,
          render("^\\p{InGreek}+$").isEmpty
        )
    )
  )
