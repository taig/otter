package io.taig.otter

import io.circe.Json

object CirceJsonRecordEncoder:
  def apply[A](codec: Record[?, A], a: A): List[(String, Json)] = codec match
    case Record.Empty(_)            => Nil
    case Record.Modify(self, _, g)  => CirceJsonRecordEncoder(self, g(a))
    case Record.Root(field, _)      => CirceJsonFieldEncoder(field, a).toList
    case Record.Zip(left, right, _) => CirceJsonRecordEncoder(left, a._1) ++ CirceJsonRecordEncoder(right, a._2)
