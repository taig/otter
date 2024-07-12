package io.taig.otter

object ValidationWriterValueStringEncoder:
  def apply[F, A](writer: ValidationWriter.Value[A]): Option[String] = writer match
    case ValidationWriter.Value.Root(writer, value) => ValueStringEncoder(writer, value)
