package io.taig.otter.codec

import io.taig.otter.Coerce
import scala.annotation.tailrec

final class CoercePrinter[-S[_]](printer: Printer[S]) extends Printer[Coerce[S, *]]:
  @tailrec
  override def print[A](schema: Coerce[S, A], a: A): String = schema match
    case Coerce.Modify(self, _, g) => print(schema = self, g(a))
    case Coerce.Root(schema)       => printer.print(schema = schema.value, a)

object CoercePrinter:
  def apply[S[_]](printer: Printer[S]): Printer[Coerce[S, *]] = new CoercePrinter(printer)
