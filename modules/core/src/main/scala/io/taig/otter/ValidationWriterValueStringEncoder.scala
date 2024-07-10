package io.taig.otter

object ValidationWriterValueStringEncoder:
  def apply[A](writer: ValidationWriter.Value[A]): Option[String] = writer match
    case ValidationWriter.Value.Root(writer, value) => ValueStringEncoder(writer, value)
