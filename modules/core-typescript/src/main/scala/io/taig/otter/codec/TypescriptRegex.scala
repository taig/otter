package io.taig.otter.codec

import io.taig.otter.Typescript

import scala.annotation.tailrec
import scala.util.matching.Regex

/** A `java.util.regex` pattern as the JavaScript literal that reads it the same way, where there is one.
  *
  * The two flavours agree on most of what a schema is likely to say and disagree in ways that are silent rather than
  * loud. `\p{L}` is a letter to Java and the three characters `p{L}` to a JavaScript engine in its default mode, so the
  * `u` flag is always set. `(?i)` is a flag to Java and a syntax error to JavaScript, so a pattern carrying one has no
  * literal at all. What is left over -- Java's `\h`, its possessive quantifiers, its POSIX and block property names --
  * either means something else in JavaScript or means nothing, and is refused for the same reason the object and
  * uniqueness constraints are: a generated schema that validates less than the server does is safe, and one that
  * validates something else is not.
  */
object TypescriptRegex:
  /** Property names a JavaScript engine knows in `u` mode: the general categories, and the lookups that take a value.
    * Java's own spellings -- `Alpha`, `IsAlphabetic`, `javaLowerCase`, `InGreek` -- are not among them, and are refused
    * rather than guessed at.
    */
  private val Properties: Set[String] =
    Set("C", "Cc", "Cf", "Cn", "Co", "Cs") ++
      Set("L", "LC", "Ll", "Lm", "Lo", "Lt", "Lu") ++
      Set("M", "Mc", "Me", "Mn") ++
      Set("N", "Nd", "Nl", "No") ++
      Set("P", "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps") ++
      Set("S", "Sc", "Sk", "Sm", "So") ++
      Set("Z", "Zl", "Zp", "Zs")

  private val Values: Set[String] = Set("General_Category", "Script", "Script_Extensions", "gc", "sc", "scx")

  /** Escapes JavaScript spells the way Java does. `\v` is missing on purpose: Java reads it as a class of vertical
    * whitespace and JavaScript as the one vertical tab, which is exactly the quiet disagreement this refuses.
    */
  private val Escapes: Set[Char] =
    Set('b', 'B', 'd', 'D', 's', 'S', 'w', 'W') ++
      Set('n', 'r', 't', 'f', '0') ++
      Set('\\', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$', '/', '-')

  /** An escape, which is passed through whole, or the `/` that would otherwise end the literal. */
  private val Delimiter: Regex = """\\.|/""".r

  def apply(pattern: String): Option[Typescript.Expression] =
    Option.when(isTranslatable(pattern, index = 0))(Typescript.Expression.Symbol(s"/${escaped(pattern)}/u"))

  /** A `/` closes the literal, so one that is meant as a character says so. Every other escape is already the author's
    * and is left as it was written.
    */
  private def escaped(pattern: String): String = Delimiter.replaceAllIn(
    pattern,
    matched => Regex.quoteReplacement(if matched.matched == "/" then "\\/" else matched.matched)
  )

  @tailrec
  private def isTranslatable(pattern: String, index: Int): Boolean =
    if index >= pattern.length then true
    else
      pattern.charAt(index) match
        case '\\' =>
          escape(pattern, index) match
            case Some(next) => isTranslatable(pattern, next)
            case None       => false
        case '(' if isGroup(pattern, index)                                 => false
        case '*' | '+' | '?' | '}' if pattern.lift(index + 1).contains('+') => false
        case _                                                              => isTranslatable(pattern, index + 1)

  /** Where the escape beginning at `index` ends, or nothing when JavaScript does not read it the way Java wrote it. */
  private def escape(pattern: String, index: Int): Option[Int] = pattern
    .lift(index + 1)
    .flatMap:
      case 'p' | 'P'                                    => property(pattern, index + 2)
      case 'u' | 'x' | 'c'                              => Some(index + 2)
      case 'k' if pattern.lift(index + 2).contains('<') => Some(index + 2)
      case character if character.isDigit               => Some(index + 2)
      case character if Escapes.contains(character)     => Some(index + 2)
      case _                                            => None

  /** A `\p{…}` whose name JavaScript also knows, ending after the brace that closes it. */
  private def property(pattern: String, index: Int): Option[Int] = Option
    .when(pattern.lift(index).contains('{'))(pattern.indexOf('}', index))
    .filter(_ > index)
    .flatMap: close =>
      val name = pattern.substring(index + 1, close)
      Option.when(Properties.contains(name) || Values.exists(value => name.startsWith(value + "=")))(close + 1)

  /** A group JavaScript has no counterpart for: an inline flag, and the atomic group that shares its opening. */
  private def isGroup(pattern: String, index: Int): Boolean = pattern.lift(index + 1).contains('?') &&
    pattern
      .lift(index + 2)
      .exists:
        case '>'       => true
        case character => "idmsuxU-".contains(character)
