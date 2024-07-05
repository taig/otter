package io.taig.otter

import io.taig.otter.ValidationWriter

object ValidationWriterStringEncoder:
  def apply[A](writer: ValidationWriter[A]): String = writer match
    case ValidationWriter.Root(writer, value) => ??? // StringEncoder(writer, value)
