package io.taig.otter

import io.circe.Json

object CirceJsonTupleEncoder:
  def apply[A](codec: Tuple[?, A], a: A): List[Json] = codec match
    case _: Tuple.Empty                => Nil
    case Tuple.Modify(self, _, g)      => CirceJsonTupleEncoder(self, g(a))
    case Tuple.Prepend(self, codec, _) => CirceJsonCodecEncoder(codec, a.head) :: CirceJsonTupleEncoder(self, a.tail)
    case Tuple.Root(codec, _)          => List(CirceJsonCodecEncoder(codec, a))
    case Tuple.Zip(left, right, _)     => List(CirceJsonCodecEncoder(left, a._1), CirceJsonCodecEncoder(right, a._2))
