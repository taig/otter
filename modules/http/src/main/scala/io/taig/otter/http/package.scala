package io.taig.otter.http

import cats.syntax.all.*
import java.nio.charset.Charset

private def loadCharset(name: String): Option[Charset] =
  try Charset.forName(name).some
  catch {
    case _: IllegalArgumentException => none
    // Scala.js chokes on this
    // case _: IllegalCharsetNameException => none
  }
