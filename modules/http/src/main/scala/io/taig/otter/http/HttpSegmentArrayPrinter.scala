package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import scala.annotation.tailrec
import io.taig.otter.Collection.Linked
import io.taig.otter.Collection.Modify
import io.taig.otter.Tuple.Empty
import io.taig.otter.Tuple.Root
import io.taig.otter.Tuple.Zip
import cats.data.Chain

// TODO escape
object HttpSegmentArrayPrinter:
  def apply[A](codec: Http.Segment.Array[A], a: A): Chain[String] = codec match
    case Http.Segment.Array.Collection(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Segment.Array.Tuple(self)      => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Collection[Http.Segment.Value, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(HttpSegmentValuePrinter(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(HttpSegmentValuePrinter(codec = codec.value, _))
    case Collection.Modify(self, _, g)         => apply(codec = self, g(a))

  def apply[A](codec: Tuple[Http.Segment.Value, A], a: A): Chain[String] = codec match
    case Tuple.Empty(_)            => Chain.empty
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Root(codec, _)      => Chain.one(HttpSegmentValuePrinter(codec = codec.value, a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
