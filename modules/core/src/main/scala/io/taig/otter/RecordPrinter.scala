package io.taig.otter

import cats.data.Chain

final class RecordPrinter[S[_]](printer: Printer[S]):
  def apply[A](codec: Record[S, S, A], a: A): Chain[(String, String)] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, codec, _) =>
      val name = printer(codec = key.self.value, key.value)
      val value = printer(codec = codec.value, a)
      Chain.one((name, value))
    case Record.Modify(self, f, g)  => apply(codec = self, g(a))
    case Record.Optional(self)      => a.fold(Chain.empty)(apply(codec = self, _))
    case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
