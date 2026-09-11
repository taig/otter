package io.taig.otter.http.codec

import cats.data.Chain

import scala.annotation.tailrec

/** How a repetition written as one line spells an element that the delimiter would otherwise swallow.
  *
  * A comma separated line has no grammar of its own: every character an element holds is a character the line holds, so
  * an element carrying the delimiter is read back as two. Joining and splitting alone therefore do not round trip, and
  * the write side is where that has to be answered -- `a,b` and `a`, `b` are the same line, so no reader can tell them
  * apart afterwards.
  *
  * An element is quoted only when writing it plainly would lose it: when it is empty, holds the delimiter, holds a
  * quote or a backslash, or begins or ends with whitespace. Everything else is written exactly as it was, so the
  * ordinary line is the ordinary line -- `en,de` stays `en,de` -- and quoting appears only where the alternative is a
  * value that does not survive.
  *
  * This is `style: simple` with an escape hatch OpenAPI has no way to write down, which is the price of a header that
  * holds what it was given. A generated client that joins on the delimiter and nothing more still agrees on every value
  * that has no quoting to do.
  */
private[codec] object DelimitedText:
  private val Quote = '"'

  private val Escape = '\\'

  /** The one line a repetition contributes, with the elements that need it quoted. */
  def write(delimiter: String, values: Chain[String]): String =
    values.toList.map(value => if enquoted(delimiter, value) then quote(value) else value).mkString(delimiter)

  /** The elements a line holds, which is the inverse of what [[write]] writes.
    *
    * Whitespace around the delimiter is dropped from an unquoted element, because a header set is read from the open
    * world and RFC 9110 allows it there. Inside quotes it is the value and is kept.
    */
  def read(delimiter: String, value: String): Chain[String] =
    if value.forall(_.isWhitespace) then Chain.empty else elements(delimiter, value, index = 0, Chain.empty)

  /** Whether writing the value plainly would lose it. */
  private def enquoted(delimiter: String, value: String): Boolean =
    value.isEmpty || value.contains(delimiter) || value.exists(character =>
      character == DelimitedText.Quote || character == DelimitedText.Escape
    ) || value != value.trim

  private def quote(value: String): String =
    val builder = new StringBuilder(value.length + 2)
    builder.append(DelimitedText.Quote)

    value.foreach: character =>
      if character == DelimitedText.Quote || character == DelimitedText.Escape then builder.append(DelimitedText.Escape)
      builder.append(character)

    builder.append(DelimitedText.Quote).toString

  @tailrec
  private def elements(delimiter: String, value: String, index: Int, accumulator: Chain[String]): Chain[String] =
    val start = DelimitedText.whitespace(value, index)

    val quoted =
      if start < value.length && value.charAt(start) == DelimitedText.Quote then DelimitedText.unquote(value, start + 1)
      else None

    val (element, continuation) = quoted match
      case Some((element, next)) => (element, value.indexOf(delimiter, next))
      case None                  =>
        val next = value.indexOf(delimiter, start)
        val raw = if next < 0 then value.substring(start) else value.substring(start, next)
        (raw.trim, next)

    if continuation < 0 then accumulator :+ element
    else elements(delimiter, value, continuation + delimiter.length, accumulator :+ element)

  /** The value a quoted run holds and the index after its closing quote, or nothing when it is never closed.
    *
    * A run left open is not this grammar, so it falls back to being read plainly -- the same elements the delimiter
    * alone would have found. Reporting it instead would refuse a header over a stray quote, and what [[write]] writes
    * is always closed, so nothing this library sends takes that path.
    */
  private def unquote(value: String, index: Int): Option[(String, Int)] =
    DelimitedText.unquoting(value, index, new StringBuilder)

  @tailrec
  private def unquoting(value: String, index: Int, builder: StringBuilder): Option[(String, Int)] =
    if index >= value.length then None
    else
      val character = value.charAt(index)

      if character == DelimitedText.Escape && index + 1 < value.length then
        DelimitedText.unquoting(value, index + 2, builder.append(value.charAt(index + 1)))
      else if character == DelimitedText.Quote then Some((builder.toString, index + 1))
      else DelimitedText.unquoting(value, index + 1, builder.append(character))

  @tailrec
  private def whitespace(value: String, index: Int): Int =
    if index < value.length && value.charAt(index).isWhitespace then DelimitedText.whitespace(value, index + 1)
    else index
