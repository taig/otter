package io.taig.otter

import java.io.StringWriter
import java.io.PrintWriter

private[otter] def escape(value: String, characters: List[String], escape: Char = '\\'): String =
  characters.foldLeft(value.replace(s"$escape", s"$escape$escape")): (value, character) =>
    value.replace(character, s"$escape$character")

private[otter] def escape(value: String, character: String): String = escape(value, characters = List(character))

private[otter] def unescape(value: String, characters: List[String], escape: Char = '\\'): String =
  characters
    .foldLeft(value): (value, character) =>
      value.replace(s"$escape$character", character).replace(s"$escape$escape", s"$escape")
    .replace(s"$escape$escape", s"$escape")

private[otter] def unescape(value: String, character: String): String =
  unescape(value, characters = List(character))

private[otter] object StacktracePrinter:
  def apply(throwable: Throwable): String =
    val writer = new StringWriter
    throwable.printStackTrace(new PrintWriter(writer))
    writer.toString
