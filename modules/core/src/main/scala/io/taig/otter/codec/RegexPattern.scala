package io.taig.otter.codec

import java.util.regex.Pattern
import scala.annotation.tailrec

/** The conservative, unflagged subset shared by Java and ECMA-262, with full-match semantics. Unsupported syntax
  * returns nothing; callers decide how to report that loss.
  */
object RegexPattern:
  def apply(pattern: Pattern): Option[String] =
    Option.when(pattern.flags() == 0)(pattern.pattern()).flatMap(translate).map(source => s"^(?:$source)(?![\\s\\S])")

  private def translate(source: String): Option[String] =
    val output = new StringBuilder
    @tailrec def loop(index: Int, inClass: Boolean): Option[String] =
      if index >= source.length then Option.when(!inClass)(output.toString)
      else
        source.charAt(index) match
          case '\\' =>
            if index + 1 >= source.length then None
            else
              val escaped = source.charAt(index + 1)
              if "dDwnrtf\\.*+?()[]{}|^$/".contains(escaped) then
                val _ = output.append('\\').append(escaped)
                loop(index + 2, inClass)
              else None
          case '[' =>
            if inClass || source.lift(index + 1).contains(']') then None
            else
              val _ = output.append('[')
              loop(index + 1, inClass = true)
          case ']' =>
            if !inClass then None
            else
              val _ = output.append(']')
              loop(index + 1, inClass = false)
          case '&' if inClass  => None
          case '.' if !inClass =>
            val _ = output.append("[^\\r\\n\\u0085\\u2028\\u2029]")
            loop(index + 1, inClass)
          case '(' if !inClass && source.lift(index + 1).contains('?') =>
            if source.lift(index + 2).contains(':') then
              val _ = output.append("(?:")
              loop(index + 3, inClass)
            else None
          case '{' if !inClass =>
            val end = source.indexOf('}', index)
            if end < 0 || !source.substring(index + 1, end).matches("[0-9]+(,[0-9]*)?") ||
              source.lift(end + 1).contains('+')
            then None
            else
              val _ = output.append(source.substring(index, end + 1))
              loop(end + 1, inClass)
          case '}' if !inClass                                                     => None
          case '*' | '+' | '?' if !inClass && source.lift(index + 1).contains('+') => None
          case '^' if !inClass && index != 0                                       => None
          case '$' if !inClass && index != source.length - 1                       => None
          case character if character < ' ' || character > '~'                     => None
          case character                                                           =>
            val _ = output.append(character)
            loop(index + 1, inClass)
    loop(0, inClass = false)
