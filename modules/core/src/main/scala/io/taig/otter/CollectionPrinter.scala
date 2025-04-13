package io.taig.otter

import scala.annotation.tailrec

final class CollectionPrinter[S[_]](printer: Printer[S]):
  @tailrec
  def apply[A](codec: Collection[S, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(printer(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(printer(codec = codec.value, _))
    case Collection.Modify(self, _, g)         => apply(codec = self, g(a))
