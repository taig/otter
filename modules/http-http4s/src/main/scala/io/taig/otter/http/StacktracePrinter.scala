package io.taig.otter.http

import java.io.StringWriter
import java.io.PrintWriter

private[otter] object StacktracePrinter:
  def apply(throwable: Throwable): String =
    val writer = new StringWriter
    throwable.printStackTrace(new PrintWriter(writer))
    writer.toString
