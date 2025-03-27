package io.taig.otter

import io.circe.Json

object CirceJsonRecordPrinter:
  def apply[A](codec: Record[?, A], a: A): List[(String, Json)] = codec match
    case Record.Empty(_)            => Nil
    case Record.Modify(self, _, g)  => CirceJsonRecordPrinter(self, g(a))
    case Record.Root(field, _)      => CirceJsonFieldPrinter(field, a).toList
    case Record.Zip(left, right, _) => CirceJsonRecordPrinter(left, a._1) ++ CirceJsonRecordPrinter(right, a._2)
