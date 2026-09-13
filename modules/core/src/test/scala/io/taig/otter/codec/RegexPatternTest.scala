package io.taig.otter.codec

import zio.Scope
import zio.test.*

import java.util.regex.Pattern

object RegexPatternTest extends ZIOSpecDefault:
  override val spec: Spec[TestEnvironment & Scope, Any] = suite("RegexPatternTest")(
    test("supported patterns retain full-match behavior, including line terminators"):
      val patterns = List("abc", "a|b", "a.*b", "[a-z]+", "a{1,3}", "\\d+", "^abc$")
      val values = List("", "abc", "xxabcxx", "abc\n", "abc\r\n", "a", "b", "ab", "a\u0085b", "a\u2028b", "123")
      assertTrue(patterns.forall: source =>
        val original = Pattern.compile(source)
        RegexPattern(original).exists: translated =>
          val target = Pattern.compile(translated)
          values.forall(value => original.matcher(value).matches() == target.matcher(value).find()))
    ,
    test("flags, class intersections, backreferences and nonportable escapes are omitted"):
      val patterns = List(
        Pattern.compile("abc", Pattern.CASE_INSENSITIVE),
        Pattern.compile("abc", Pattern.LITERAL),
        Pattern.compile("a.*", Pattern.DOTALL),
        Pattern.compile("(?i)abc"),
        Pattern.compile("a++"),
        Pattern.compile("[a-z&&[^b]]"),
        Pattern.compile("(a)\\1"),
        Pattern.compile("\\s+"),
        Pattern.compile("\\babc")
      )
      assertTrue(patterns.forall(pattern => RegexPattern(pattern).isEmpty))
  )
