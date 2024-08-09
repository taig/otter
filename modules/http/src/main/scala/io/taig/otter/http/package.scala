package io.taig.otter.http

import cats.syntax.all.*
import java.nio.charset.Charset
import java.nio.charset.UnsupportedCharsetException
import java.nio.charset.IllegalCharsetNameException

private def loadCharset(name: String): Option[Charset] =
  try Charset.forName(name).some
  catch {
    case _: UnsupportedCharsetException => none
    case _: IllegalCharsetNameException => none
  }
