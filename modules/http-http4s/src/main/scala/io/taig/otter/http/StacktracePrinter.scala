package io.taig.otter.http

import java.io.PrintWriter
import java.io.StringWriter

private[otter] object StacktracePrinter:
  def apply(throwable: Throwable): String =
    val writer = new StringWriter
    throwable.printStackTrace(new PrintWriter(writer))
    writer.toString
